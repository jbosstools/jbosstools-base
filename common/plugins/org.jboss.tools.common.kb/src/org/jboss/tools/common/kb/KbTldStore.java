/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.kb;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * Describes store whish contains all TLD schemas.
 * @author eskimo
 */
public class KbTldStore implements KbStore {

	private String schemaLocation;
//	private String jspSchemaLocation;
	private HashMap<KbTldResource,Document> activeDocuments;
	private HashMap<KbTldResource,KbTldResource> registratedResources;
//	private HashMap registratedDinamicResources;
	private HashMap<KbTldResource,KbTldResource> loadingResources;

	private static final KbTldStore INSTANCE = new KbTldStore();

	private static Map<String,TagDescriptor> tagInfoCache = new HashMap<String,TagDescriptor>(1000);
	private static Map<String,TagDescriptor> faceletsHtmlTagInfoCache = new HashMap<String,TagDescriptor>(1000);
	private static Map<String,HashMap<String,TldElement>> tldElementsByPrefix = new HashMap<String,HashMap<String,TldElement>>(1000);
	private static Map<String,HashMap<String,TldElement>> tldFaceletsHtmlElementsByPrefix = new HashMap<String,HashMap<String,TldElement>>(1000);
	private static Map<String,ArrayList<TldElement>> tldElementsByName = new HashMap<String,ArrayList<TldElement>>(1000);

	private KbTldStore() {
		schemaLocation = KbConfigurationFactory.getInstance().getDefaultConfiguration().getTldCustomSchemaPath();
//		jspSchemaLocation = KbConfigurationFactory.getInstance().getDefaultConfiguration().getJspSchemaFilePath();

		activeDocuments = new HashMap<KbTldResource,Document>();
		registratedResources = new HashMap<KbTldResource,KbTldResource>();
//		registratedDinamicResources = new HashMap();
		loadingResources = new HashMap<KbTldResource,KbTldResource>();
		loadRegistratedResources();
	}

	/**
	 * Clears cach of tag descriptors.
	 */
	public void clearCache() {
		tagInfoCache.clear();
		tldElementsByPrefix.clear();
	}

	/**
	 * Clears html facelets cach of tag descriptors.
	 */
	public void clearFaceletsHtmlCash() {
		faceletsHtmlTagInfoCache.clear();
		tldFaceletsHtmlElementsByPrefix.clear();
	}

	private static Map<String,TagDescriptor> getTagInfoCache() {
	    if(tagInfoCache.size()>1000) {
	        tagInfoCache.clear();
	    }
	    return tagInfoCache;
	}

	private static Map<String,TagDescriptor> getFaceletsHtmlTagInfoCache() {
	    if(faceletsHtmlTagInfoCache.size()>1000) {
	    	faceletsHtmlTagInfoCache.clear();
	    }
	    return faceletsHtmlTagInfoCache;
	}

	private static Map<String,HashMap<String,TldElement>> getTldElementsByPrefix() {
	    if(tldElementsByPrefix.size()>1000) {
	    	tldElementsByPrefix.clear();
	    }
	    return tldElementsByPrefix;
	}

	private static Map<String,HashMap<String,TldElement>> getFaceletsHtmlTldElementsByPrefix() {
	    if(tldFaceletsHtmlElementsByPrefix.size()>1000) {
	    	tldFaceletsHtmlElementsByPrefix.clear();
	    }
	    return tldFaceletsHtmlElementsByPrefix;
	}

	private static Map<String,ArrayList<TldElement>> getTldElementsByName() {
	    if(tldElementsByName.size()>1000) {
	    	tldElementsByName.clear();
	    }
	    return tldElementsByName;
	}

	/**
	 * @return Instance of Store.
	 */
	public static KbTldStore getInstance() {
		return INSTANCE;
	}

	/**
	 * Returns Tag Descriptor for query.
	 * @param query
	 * @return Tag Descriptor for query.
	 * @see org.jboss.tools.common.kb.KbStore#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor queryTagInformation(KbQuery query) {
		String cleanQuary = query.getFullLastTagName();

		Collection<KbTldResource> needResources = convertQueryTldResourceToRegistretedTldResource(query.getResources());

		String key = needResources.hashCode() + cleanQuary;

		boolean faceletsHtml = cleanQuary.indexOf("0fHP:")>-1;
		Object o = faceletsHtml?getFaceletsHtmlTagInfoCache().get(key):getTagInfoCache().get(key);
		if(o!=null) {
    	  	return (TagDescriptor)o;
      	}

		String tagName = query.getLastTagName();
		if(tagName==null) {
			return null;
		}
		String prefixName = query.getLastTagPrefixName();
		if(prefixName == null) {
			return null;
		}

		activateResources(needResources);

		if(!checkQuery(query.getQuery())) {
			return null;
		}

		ArrayList tldElementTypes = getTldElementsByName(needResources, prefixName, tagName, false);
		if(tldElementTypes.size()==0) {
			return null;
		}
		Element elementType = ((TldElement)tldElementTypes.get(0)).getElement();

		Collection dinamicResources = query.getDinamicResources();
		Set<String> types = new HashSet<String>();
		for (Iterator iter = dinamicResources.iterator(); iter.hasNext();) {
			KbDinamicResource resource = (KbDinamicResource) iter.next();
			String type = resource.getType();
			types.add(type);
		}
		types.add(KbDinamicResource.JSF_VARIABLES_TYPE);
		types.add(SchemaNodeFactory.ENUMERATION_TYPE);

		TagDescriptor tagInfo = KbSchemaUtil.getTagInformationFromElementType(elementType, types);
		if(tagInfo!=null) {
			tagInfo.setPrefix(prefixName);
		}

		if(faceletsHtml) {
			getFaceletsHtmlTagInfoCache().put(key, tagInfo);
		} else {
			getTagInfoCache().put(key, tagInfo);
		}
		return tagInfo;
	}

	/**
	 * Returns Attribute Descriptor for query.
	 * @param query
	 * @return
	 */
	public AttributeDescriptor queryAttributeInformation(KbQuery query) {
		TagDescriptor tagDesriptor = queryTagInformation(query);
		if(tagDesriptor==null) {
			return null;
		}
		int startAttributeName = query.getQuery().indexOf(KbQuery.ATTRIBUTE_SEPARATOR);
		if(startAttributeName<0) {
			return null;
		}
		startAttributeName += KbQuery.ATTRIBUTE_SEPARATOR.length();
		String attributeName = query.getQuery().substring(startAttributeName);
		return tagDesriptor.getAttributeDescriptor(attributeName);
	}

	/**
 	 * @param query
	 * @return Collection of proposal from store. 
	 * @see org.jboss.tools.common.kb.KbStore#queryProposal(org.jboss.tools.common.kb.KbQuery)
	 */
	public Collection<KbProposal> queryProposal(KbQuery query) {
		ArrayList<KbProposal> proposals = queryTagProposal(query);
//		Collections.sort(proposals);

		return proposals;
	}

	private boolean checkQuery(String strQuery) {
		if((!strQuery.startsWith(KbQuery.TAG_SEPARATOR))||
//				   (strQuery.startsWith(KbQuery.TAG_SEPARATOR + KbQuery.PREFIX_SEPARATOR))||
				   (strQuery.indexOf(KbQuery.PREFIX_SEPARATOR + KbQuery.ATTRIBUTE_SEPARATOR)!=-1)||
				   (strQuery.indexOf(KbQuery.ATTRIBUTE_SEPARATOR + KbQuery.ENUMERATION_SEPARATOR)!=-1)||
				   (strQuery.startsWith(KbQuery.TAG_SEPARATOR + KbQuery.ATTRIBUTE_SEPARATOR))) {
//					String errorMessage = "ERROR: Bad query: " + strQuery + "\n" + 
//										  "       Possible query format: \"" + KbQuery.TAG_SEPARATOR +"prefixName" + KbQuery.PREFIX_SEPARATOR + "tagName" + KbQuery.ATTRIBUTE_SEPARATOR + "attributeName" + KbQuery.ENUMERATION_SEPARATOR + "attributeValue\"";
					return false;
		}
		return true;
	}

	private ArrayList<KbProposal> queryTagProposal(KbQuery query) {
		String strQuery = query.getQuery();

		Collection needResources = convertQueryTldResourceToRegistretedTldResource(query.getResources());
		activateResources(needResources);

		if(!checkQuery(strQuery)) {
			return new ArrayList<KbProposal>();
		}

		if(strQuery.length() == KbQuery.TAG_SEPARATOR.length()) {
			return getTags(needResources, "");
		}

		int startTagName = strQuery.indexOf(KbQuery.PREFIX_SEPARATOR);
		if(startTagName < 0){
			return getTags(needResources, strQuery.substring(KbQuery.TAG_SEPARATOR.length()));
		}

		String prefixName = strQuery.substring(KbQuery.TAG_SEPARATOR.length(), startTagName);
		startTagName+=KbQuery.PREFIX_SEPARATOR.length();
		int startAttributeName = strQuery.indexOf(KbQuery.ATTRIBUTE_SEPARATOR);
		if(startAttributeName < 0){
			String tagName = strQuery.substring(startTagName);
			boolean tagMask = true;
			if(tagName.endsWith("/")) {
				tagMask = false;
				tagName = tagName.substring(0, tagName.length()-1);
			}
			return getTags(needResources, prefixName, tagName, tagMask);
		}

		String tagName = strQuery.substring(startTagName, startAttributeName);
		startAttributeName+=KbQuery.ATTRIBUTE_SEPARATOR.length();
		if(startAttributeName == strQuery.length()) {
			return getAttributes(needResources, prefixName, tagName, "");
		}

		int startAttributeValue = strQuery.indexOf(KbQuery.ENUMERATION_SEPARATOR);
		if(startAttributeValue < 0) {
			return getAttributes(needResources, prefixName, tagName, strQuery.substring(startAttributeName));
		}

		String attributeName = strQuery.substring(startAttributeName, startAttributeValue);
		startAttributeValue+=KbQuery.ENUMERATION_SEPARATOR.length();

//		Collection needDinamicResources = convertQueryDinamicResourceToRegistretedDinamicResource(query.getDinamicResources());
		Collection needDinamicResources = query.getDinamicResources();
		if(startAttributeValue == strQuery.length()) {
			return getAttributeValue(needResources, needDinamicResources, prefixName, tagName, attributeName, "");
		}

		return getAttributeValue(needResources, needDinamicResources, prefixName, tagName, attributeName, KbQuery.decode(strQuery.substring(startAttributeValue)));
	}
/*
	private String getLastTag(String query) {
		StringTokenizer tags = new StringTokenizer(query, KbQuery.TAG_SEPARATOR, true);
		String lastTag = null;

		while(tags.hasMoreTokens()) {
			String tag = tags.nextToken();
			if(tag.equals(KbQuery.TAG_SEPARATOR)) {
				continue;
			}
			if(tags.hasMoreTokens()) {
				lastTag = tag;
			}
		}

		return lastTag;
	}
*/
	private Collection<KbTldResource> convertQueryTldResourceToRegistretedTldResource(Collection resources) {
		ArrayList<KbTldResource> regResources = new ArrayList<KbTldResource>(resources.size());
		for(Iterator iter = resources.iterator(); iter.hasNext();) {
			KbTldResource queryResource = (KbTldResource)iter.next();

			KbTldResource regResource = getRegistratedResource(queryResource);
			if((regResource!=null)&&(!regResources.contains(regResource))) {
				regResource.setPrefixes(queryResource.getPrefixes());
				regResources.add(regResource);
			}
		}
		return regResources;
	}
/*
	private Collection convertQueryDinamicResourceToRegistretedDinamicResource(Collection resources) {
		ArrayList regResources = new ArrayList(resources.size());
		for(Iterator iter = resources.iterator(); iter.hasNext();) {
			KbDinamicResource queryResource = (KbDinamicResource)iter.next();
			KbDinamicResource regResource = (KbDinamicResource)registratedDinamicResources.get(queryResource);
			if((regResource!=null)&&(!regResources.contains(regResource))) {
				regResources.add(regResource);
			}
		}
		return regResources;
	}
*/
	/**
	 * Register resource
	 * @param resource
	 * @param waitForEndRegistration
	 * @return registred resource
	 */
	public synchronized KbResource registerResource(KbResource resource, boolean waitForEndRegistration) {
		KbTldResource tldResource = null;
		if(resource instanceof KbTldResource) {
			tldResource = (KbTldResource)resource;
			KbTldResource regResource = getRegistratedResource(tldResource);
			if(regResource!=null && regResource.isModified(tldResource.getTldContent())) {
				unregisterResource(resource, true);
				return registerResource(tldResource, waitForEndRegistration, true);
			} else {
				return registerResource(tldResource, waitForEndRegistration, false);
			}
		} else if(resource instanceof KbDinamicResource) {
//			return registerDinamicResource((KbDinamicResource)resource);
			return (KbDinamicResource)resource;
		} else {
			throw new RuntimeException("KbTldStore.registerResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}
	}

	/**
	 * Registers resource in store
	 * @param resource
	 * @see org.jboss.tools.common.kb.KbStore#registerResource(org.jboss.tools.common.kb.KbResource)
	 */
	public synchronized void registerResource(KbResource resource) {
		registerResource(resource, false);
	}

	private synchronized KbResource registerResource(KbTldResource tldResource, boolean waitForEndRegistration, boolean modifiedResource) {
		KbTldResource regResource = getRegistratedResource(tldResource);

		if(regResource!=null && !modifiedResource) {
			regResource.addPrefix(tldResource.getFirstPrefix());
			activateResource(tldResource);
			return regResource;
		}

		if(checkDownloadingResource(tldResource)) {
			// Resource is downloading now
			return tldResource;
		}

		// Start downloading resource
		if(waitForEndRegistration) {
			// Don't start new thread.
			ResourceLoader rl = new ResourceLoader(tldResource);
			rl.run();
			if(rl.resourceHasLoaded()) {
				return tldResource;
			}
		} else {
			new ResourceLoader(tldResource).start();
		}
		return null;
	}
/*
	private synchronized KbResource registerDinamicResource(KbDinamicResource resource) {
		if(!checkDinamicRegistratedResource(resource)) {
			registratedDinamicResources.put(resource, resource);
			return resource;
		}
		return null;
	}
*/
	/**
	 * Unregisters resource
	 * @param resource
	 * @param removeSchema
	 */
	public synchronized void unregisterResource(KbResource resource, boolean removeSchema) {
		if(resource instanceof KbDinamicResource) {
//			unregisterDinamicResource((KbDinamicResource)resource);
			return;
		} else if(resource instanceof KbTldResource) {
			KbTldResource tldResource = (KbTldResource)resource;
			KbTldResource regResource = getRegistratedResource(tldResource);
			if(regResource!=null) {
				regResource.removePrefix(tldResource.getFirstPrefix());
				if(regResource.getPrefixes().isEmpty() || removeSchema) { 
					registratedResources.remove(regResource);
					activeDocuments.remove(regResource);
//					Document document = (Document)activeDocuments.get(tldResource);
//					if(document!=null) {
//						activeDocuments.remove(document);
//					}
					File schema = regResource.getSchemaLocation();
					if(schema!=null) {
						schema.delete();
					}
				}
			} else if(checkDownloadingResource(tldResource)) {
//				TODO Stop loading and registration resource
			}
		} else {
			throw new RuntimeException("KbTldStore.unregisterResource(KbResource resource): resource must be instance of KbTldResource or KbDinamicResource");
		}
	}

	/**
	 * Unregisters resource
	 * @param resource
	 * @see org.jboss.tools.common.kb.KbStore#unregisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public synchronized void unregisterResource(KbResource resource) {
		unregisterResource(resource, false);
	}
/*
	private void unregisterDinamicResource(KbDinamicResource resource) {
		KbDinamicResource regResource = (KbDinamicResource)registratedDinamicResources.get(resource);
		if(regResource!=null) {
			registratedDinamicResources.remove(regResource);
		}
	}
*/
	/**
	 * Unregisters resource prefix
	 * @param resource
	 * @return
	 */
	public boolean unregisterResourcePrefix(KbTldResource resource) {
		boolean emptyPrefixList = true;
		KbTldResource regResource = getRegistratedResource(resource);
		if(regResource!=null) {
			regResource.removePrefix(resource.getFirstPrefix());
			return regResource.getPrefixes().isEmpty();
		}
		return emptyPrefixList;
	}

	/**
	 * 
	 * @param tldResource
	 * @return
	 */
	public synchronized List<String> getAllTagNamesFromResource(KbTldResource tldResource) {
	    ArrayList<String> names = new ArrayList<String>();
	    activateResource(tldResource);
	    KbTldResource resource = getRegistratedResource(tldResource);
	    if(resource==null) {
	        return names;
	    }
	    Document document = (Document)activeDocuments.get(resource);
	    if(document==null) {
	        return names;
	    }
	    NodeList list = document.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
	    for(int i=0; i<list.getLength(); i++) {
	        Element elementType = (Element)list.item(i);
	        String name = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
	        if(name!=null) {
	            names.add(name);
	        }
	    }
	    return names;
	}

	private synchronized boolean checkActiveDocument(KbTldResource resource) {
		return activeDocuments.get(resource) != null;
	}

	private synchronized boolean checkDownloadingResource(KbTldResource resource) {
		return loadingResources.get(resource) != null;
	}
/*
	private synchronized boolean checkRegistratedResource(KbTldResource resource) {
		return getRegistratedResource(resource) != null;
	}

	private synchronized boolean checkDinamicRegistratedResource(KbDinamicResource resource) {
		return registratedDinamicResources.get(resource) != null;
	}
*/
	private synchronized KbTldResource getRegistratedResource(KbTldResource tldResource) {
		synchronized (registratedResources) {
			KbTldResource resource = (KbTldResource)registratedResources.get(tldResource);
			if(resource!=null) {
				resource.mergeTldLocation(tldResource);
			} else {
				try {
					KbTldResource resourceWithoutVersion = (KbTldResource)tldResource.clone();
					resourceWithoutVersion.setVersion(null);
					resource = (KbTldResource)registratedResources.get(resourceWithoutVersion);
				} catch (CloneNotSupportedException e) {
					KbPlugin.getPluginLog().logError(e);
				}
			}
			return resource;
		}
	}

	/**
	 * 
	 * @return
	 */
	public synchronized HashMap getRegistratedResources() {
	    return this.registratedResources;
	}

	private synchronized void activateResource(KbTldResource resource) {
		if(checkActiveDocument(resource)) {
			return;
		}

		KbTldResource regResource = getRegistratedResource(resource);
		if(regResource == null) {
			return;
		}

		if(regResource.getTldContent()!=null) {
			resource.setTldContent(regResource.getTldContent());
		}

		File schemaLocation = regResource.getSchemaLocation();
		if((schemaLocation == null)||(!schemaLocation.exists())) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logWarning("WARNING: Schema (location: " + schemaLocation + ") for resource (" + regResource +") does not exist!");
			}
			return;
		}

		Document document = null;
		try {
			document = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(schemaLocation);
		} catch (Exception e) {
        	KbPlugin.getPluginLog().logError(e);
			return;
		}

		activeDocuments.put(regResource, document);
	}

	private synchronized void activateResources(Collection resources) {
		Iterator iterator = resources.iterator();
		while(iterator.hasNext()) {
			KbTldResource resource = (KbTldResource)iterator.next();
			activateResource(resource);
		}
	}

/*
	private synchronized void deactivateResource(KbTldResource resource) {
		Document document = getActiveDocument(resource);
		if(document!=null) {
			activeDocuments.remove(document);
		}
	}
*/
	/**
	 * 
	 */
	public synchronized void deactivateAllResources() {
		for(int i=0; i<activeDocuments.size(); i++) {
			activeDocuments.clear();
		}
	}

	private void loadRegistratedResources() {
		// Get the schemas from extention point.
		File kbPluginLocation = KbPlugin.getDefault().getLocation();
		if(kbPluginLocation!=null) {
	        IExtensionRegistry registry = Platform.getExtensionRegistry();
			IExtensionPoint extensionPoint = registry.getExtensionPoint("org.jboss.tools.common.kb.tldResource");
			IExtension[] extensions = extensionPoint.getExtensions();
			for (int i=0; i<extensions.length; i++) {
				IExtension extension = extensions[i];
				IConfigurationElement[] elements = extension.getConfigurationElements();
				for(int j=0; j<elements.length; j++) {
					String uri = elements[j].getAttribute("uri");
					String location = elements[j].getAttribute("schemaLocation");
					String version = elements[j].getAttribute("version");
					String jsf = elements[j].getAttribute("jsf");
					if(uri==null || uri.length()==0 || location==null || location.length()==0) {
						continue;
					}
					File shemaLocation = new File(kbPluginLocation, location);
					if(shemaLocation.isFile()) {
						KbTldResource resource = new KbTldResource(uri, null, null, null);
						resource.setSchemaLocation(shemaLocation);
						if(version!=null && version.length()>0) {
							resource.setVersion(version);
						}
						resource.setCustomTld(false);
						resource.setJsfResource("true".equals(jsf));
						registratedResources.put(resource, resource);
					} else {
						String message = "Can't load KB schema: " + shemaLocation;
						KbPlugin.getDefault().getLog().log(new Status(IStatus.WARNING, KbPlugin.PLUGIN_ID, IStatus.WARNING, message, null));
					}
				}
			}
		}

		// Get custom schemas.
		File schemaFolder = new File(schemaLocation); 
		if(!schemaFolder.exists()) {
			return;
		}
		File[] schemas = schemaFolder.listFiles(new FileFilter(){
			public boolean accept(File file) {
				if(file.isFile()) {
					return true;
				}
				return false;
			}
		});

		for(int i=0; i<schemas.length; i++) {
			Document document = null;
			try {
				document = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(schemas[i]);
			} catch (Exception e) {
				KbPlugin.getDefault().getLog().log(new Status(IStatus.ERROR, KbPlugin.PLUGIN_ID, IStatus.OK, "Can't parse Schema (location: " + schemas[i] + ")", e));
				continue;
			}
//			String tldLocation = schemas[i].getAbsolutePath();
			String tldLocation = document.getDocumentElement().getAttribute(SchemaNodeFactory.LOCATION_ATTRIBUTE);
			String tldUri = document.getDocumentElement().getAttribute(SchemaNodeFactory.URI_ATTRIBUTE);
			String tldVersion = document.getDocumentElement().getAttribute(SchemaNodeFactory.VERSION_ATTRIBUTE);
			String jsf = document.getDocumentElement().getAttribute(SchemaNodeFactory.JSF_ATTRIBUTE);
			String tldContent = null; 
			NodeList children = document.getDocumentElement().getChildNodes();
			for(int j=0; j<children.getLength(); j++) {
				Node node = children.item(j);
				if(node.getNodeName().equals(SchemaNodeFactory.TLD_CONTENT_NODE)) {
					Node child = node.getFirstChild();
					if(child instanceof CDATASection) {
						tldContent = ((CDATASection)child).getData();
					}
					break;
				}
			}
			KbTldResource resource = new KbTldResource(tldUri, tldLocation, null, tldVersion);
			resource.setTldContent(tldContent);
			resource.setSchemaLocation(schemas[i]);
			resource.setCustomTld(true);
			resource.setJsfResource("true".equals(jsf));
			registratedResources.put(resource, resource);
		}
	}

	/**
	 * @return
	 */
	public KbResource getJspResource() {
		return getRegistratedResource(new KbTldResource("http://java.sun.com/JSP/Page", "", "jsp", null));
	}

	private ArrayList<KbProposal> getTags(Collection resources, String prefixMask) {
		return getProposal(getFirstTldElementsByPrefix(resources, prefixMask));
	}

	private ArrayList<KbProposal> getTags(Collection resources, String prefixName, String tagName, boolean tagMask) {
		return getProposal(getTldElementsByName(resources, prefixName, tagName, tagMask));
	}

	private ArrayList<KbProposal> getAttributeValue(Collection resources, Collection dinamicResources, String prefixName, String tagName, String attributeName, String valueMask) {
		ArrayList<KbProposal> proposals = new ArrayList<KbProposal>();

		ArrayList tldAttributeTypes = getAttributeTypesByName(resources, prefixName, tagName, attributeName, false);
		if(!tldAttributeTypes.isEmpty()) {
			TldElement tldAttributeType = (TldElement)tldAttributeTypes.get(0);
			Element attributeType = tldAttributeType.getElement();
			proposals = getAttributeValueProposals(attributeType, dinamicResources, valueMask);
		}

		return proposals;
	}

	private ArrayList<KbProposal> getAttributeValueProposals(Element attributeType, Collection dinamicResources, String valueMask) {
		ArrayList<KbProposal> proposals = new ArrayList<KbProposal>();

		NodeList nodeList = attributeType.getElementsByTagName(SchemaNodeFactory.PROPOSAL_NODE);
		synchronized(dinamicResources) {
			for(int i=0; i<nodeList.getLength(); i++) {
				Element proposalElement = (Element)nodeList.item(i);
				String type = proposalElement.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);
				if(type.equals(SchemaNodeFactory.ENUMERATION_TYPE)) {
					proposals.addAll(getEnumeration(proposalElement, valueMask));
					continue;
				}
				for(Iterator iter=dinamicResources.iterator(); iter.hasNext();) {
					KbDinamicResource resource = (KbDinamicResource)iter.next();
					if(resource.getType().equals(type)) {
						setResourceConstraints(resource, proposalElement);
						proposals.addAll(resource.queryProposal(valueMask));
					}
				}
			}
		}

		return proposals;
	}

	private void setResourceConstraints(KbDinamicResource resource, Element proposalElement) {
		NodeList nodeList = proposalElement.getElementsByTagName(SchemaNodeFactory.PARAM_NODE);
		resource.clearConstraints();
		for(int i=0; i<nodeList.getLength(); i++) {
			Element paramElement = (Element)nodeList.item(i);
			String name = paramElement.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			String value = paramElement.getAttribute(SchemaNodeFactory.VALUE_ATTRIBUTE);
			resource.setConstraint(name, value);
		}
	}
/*
	private ArrayList getProposalTypes(Element attributeType) {
		ArrayList elements = new ArrayList();

		NodeList nodeList = attributeType.getElementsByTagName(SchemaNodeFactory.PROPOSAL_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			elements.add(nodeList.item(i));
		}
		return elements;
	}
*/
	private ArrayList<KbProposal> getEnumeration(Element proposalElement, String valueMask) {
		ArrayList<KbProposal> enumeration = new ArrayList<KbProposal>();

		NodeList nodeList = proposalElement.getElementsByTagName(SchemaNodeFactory.PARAM_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			Element paramElement = (Element)nodeList.item(i);
			String value = paramElement.getAttribute(SchemaNodeFactory.VALUE_ATTRIBUTE);
			if(value.startsWith(valueMask)) {
				KbProposal proposal = new KbProposal();
				proposal.setLabel(value);
				proposal.setReplacementString(value);
				proposal.setContextInfo(null);
				proposal.setIcon(KbIcon.ENUM_ITEM);
				proposal.setPosition(value.length());

				enumeration.add(proposal);
			}
		}

		return enumeration;
	}

	private ArrayList<KbProposal> getAttributes(Collection resources, String prefixName, String tagName, String attributeMask) {
		return getProposal(getAttributeTypesByName(resources, prefixName, tagName, attributeMask, true));
	}

	private ArrayList getAttributeTypesByName(Collection resources, String prefixName, String tagName, String attributeName, boolean mask) {
		ArrayList<TldElement> tldAttributes = new ArrayList<TldElement>();

		ArrayList tldElements = getTldElementsByName(resources, prefixName, tagName, false);
		if(!tldElements.isEmpty()) {
			TldElement tldElement = (TldElement)tldElements.get(0);
			Element element = tldElement.getElement();

			boolean ignoreCase = false;
			String attr = element.getOwnerDocument().getDocumentElement().getAttribute(SchemaNodeFactory.IGNORE_CASE_ATTRIBUTE);
			ignoreCase = attr!=null && attr.equals("true");

			ArrayList attributeTypes = KbSchemaUtil.getAttributeTypes(element);
			for(int i=0; i<attributeTypes.size(); i++) {
				Element attributeType = (Element)attributeTypes.get(i);

				String attributeTypeName = attributeType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				boolean endsWithWildCard = attributeTypeName.endsWith("" + SchemaNodeFactory.WILD_CARD_CHAR);
				if(endsWithWildCard) {
					attributeTypeName = attributeTypeName.substring(0, attributeTypeName.length() - 1);
				}
				if(ignoreCase) {
					attributeTypeName = attributeTypeName.toUpperCase();
					attributeName = attributeName.toUpperCase();
				}
				
				boolean match = false;
				match = endsWithWildCard ? attributeName.startsWith(attributeTypeName) :
					mask ? attributeTypeName.startsWith(attributeName) : 
						attributeTypeName.equals(attributeName);

				if(match) {
					TldElement tldAttribute = new TldElement(tldElement.getResource(), attributeType, tldElement.getPrefix(), TldElementType.ATTRIBUTE);
					tldAttributes.add(tldAttribute);
				}
			}
		}
		return tldAttributes;
	}

	private ArrayList<KbProposal> getProposal(ArrayList tldElements) {
		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(int i=0; i<tldElements.size(); i++) {
			TldElement tldElement = (TldElement)tldElements.get(i);
			Element element = tldElement.getElement();

			KbProposal proposal = new KbProposal();

			if(tldElement.isPrefix()) {
				String prefix = tldElement.getPrefix();
				String lb = prefix + KbQuery.PREFIX_SEPARATOR;

				proposal.setLabel(lb);
				proposal.setReplacementString(lb);
				proposal.setIcon(KbIcon.TLD_TAG);
			} else if(tldElement.isTag()) {
				String prefix = tldElement.getPrefix();
				String label = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				if(label == null || label.indexOf(SchemaNodeFactory.WILD_CARD_CHAR) >= 0) continue;

				String endTag = SchemaNodeFactory.REFUSED_BODY_CONTENT_TYPE.equals(element.getAttribute(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE))?" /":"";
				StringBuffer lb = new StringBuffer();
				lb.append(prefix);
				lb.append(KbQuery.PREFIX_SEPARATOR);
				lb.append(label);

				proposal.setLabel(lb.toString());

				String attributes = KbSchemaUtil.getRequaredAttributesAsString(element);
				lb.append(attributes);
				lb.append(endTag);

				proposal.setReplacementString(lb.toString());
				int position = proposal.getReplacementString().indexOf('"');
				if(position!=-1) {
					position ++;
				} else {
					position = proposal.getReplacementString().length();
				}
				proposal.setPosition(position);
				proposal.setIcon(KbIcon.TLD_TAG);
			} else {
				String label = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);

				proposal.setReplacementString(label);
				proposal.setLabel(label);
				if(KbSchemaUtil.checkRequaredAttribute(element)) {
					proposal.setIcon(KbIcon.TLD_ATTRIBUTE);
				} else {
					proposal.setIcon(KbIcon.TLD_ATTRIBUTE_OPTIONAL);
				}
			}
			proposal.setContextInfo(KbSchemaUtil.getDescription(element));

			kbProposals.add(proposal);
		}

		return kbProposals;
	}

	private ArrayList getFirstTldElementsByPrefix(Collection resources, String prefix) {
		ArrayList<TldElement> tldElements = new ArrayList<TldElement>();
		for(Iterator iterator = resources.iterator(); iterator.hasNext();) {
			KbTldResource resource = (KbTldResource)iterator.next();
			Document document = (Document)activeDocuments.get(resource);
			if(document!=null){
				ArrayList<String> uniqPrefixes = new ArrayList<String>();
				ArrayList prefixes = resource.getPrefixes();
				for(Iterator iter = prefixes.iterator(); iter.hasNext();) {
					String schemaPrefix = (String)iter.next();
					boolean uniq = true;
					for(Iterator uIter = uniqPrefixes.iterator(); uIter.hasNext();) {
						if(uIter.next().equals(schemaPrefix)) {
							uniq = false;
							break;
						}
					}
					if(uniq) {
						uniqPrefixes.add(schemaPrefix);
						if(schemaPrefix.startsWith(prefix)) {
// For Facelets
//							TldElement tldElement = new TldElement(resource, document.getDocumentElement(), schemaPrefix, TldElementType.PREFIX);
//							tldElements.add(tldElement);
							tldElements.addAll(getTldElementsByPrefix(resources, schemaPrefix, false).values());
//
						}
					}
				}
			}
		}
		return tldElements;
	}

	private HashMap<String,TldElement> getTldElementsByPrefix(Collection resources, String prefix, boolean mask) {
		String cacheKey = resources.hashCode() + "/" + prefix + "/" + mask;
		boolean faceletsHtml = prefix.equals("0fHP");
		HashMap<String,TldElement> o = faceletsHtml?getFaceletsHtmlTldElementsByPrefix().get(cacheKey):getTldElementsByPrefix().get(cacheKey);
        if(o != null) return o;

        HashMap<String,TldElement> tldElements = new HashMap<String,TldElement>();
		for(Iterator iterator = resources.iterator(); iterator.hasNext();) {
			KbTldResource resource = (KbTldResource)iterator.next();
			Document document = (Document)activeDocuments.get(resource);
			if(document!=null){

				ArrayList<String> uniqPrefixes = new ArrayList<String>();
				
				ArrayList prefixes = resource.getPrefixes();
				for(Iterator iter = prefixes.iterator(); iter.hasNext();) {
					String schemaPrefix = (String)iter.next();

					boolean uniq = true;
					for(Iterator uIter = uniqPrefixes.iterator(); uIter.hasNext();) {
						if(uIter.next().equals(schemaPrefix)) {
							uniq = false;
							break;
						}
					}
					if(uniq) {
						uniqPrefixes.add(schemaPrefix);
	
						boolean match = mask?schemaPrefix.startsWith(prefix):schemaPrefix.equals(prefix);
						if(match) {
							NodeList nodeList = document.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
							for(int j=0; j<nodeList.getLength(); j++) {
								Node child = nodeList.item(j); 
								if(child instanceof Element) {
									Element element = (Element)child;
//	For Facelets
//									TldElement tldElement = new TldElement(resource, element, schemaPrefix, TldElementType.PREFIX);
									TldElement tldElement = new TldElement(resource, element, schemaPrefix, TldElementType.TAG);
//
									tldElements.put(element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE) , tldElement);
								}
							}
						}
					}
				}
			}
		}

		if(faceletsHtml) {
			getFaceletsHtmlTldElementsByPrefix().put(cacheKey, tldElements);
		} else {
			getTldElementsByPrefix().put(cacheKey, tldElements);
		}

		return tldElements;
	}

	private ArrayList<TldElement> getTldElementsByName(Collection resources, String prefixName, String name, boolean mask) {
		String cacheKey = resources.hashCode() + "/" + prefixName + "/" + name + "/" + mask;
		ArrayList<TldElement> o = getTldElementsByName().get(cacheKey);
        if(o != null) {
            return o;
        }

		ArrayList<TldElement> tldElements = new ArrayList<TldElement>();

		HashMap tldElementTypes = getTldElementsByPrefix(resources, prefixName, false);
		if(!mask) {
			TldElement tldElementType = "0fHP".equals(prefixName)?(TldElement)tldElementTypes.get(name.toUpperCase()):(TldElement)tldElementTypes.get(name);
			if(tldElementType!=null) {
				tldElementType.setElementType(TldElementType.TAG);
				tldElements.add(tldElementType);
			}
		} else {
			Iterator iterator = tldElementTypes.values().iterator();
			boolean ignoreCase = false;
			int i=0;
			while(iterator.hasNext()) {
				TldElement tldElementType = (TldElement)iterator.next();
				Element element = tldElementType.getElement();
				if(i==0) {
					String attr = element.getOwnerDocument().getDocumentElement().getAttribute(SchemaNodeFactory.IGNORE_CASE_ATTRIBUTE);
					ignoreCase = attr!=null && attr.equals("true");
				}
				String schemaTagName = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				boolean match = false;
				if(ignoreCase) {
					match = mask?schemaTagName.toLowerCase().startsWith(name.toLowerCase()):schemaTagName.equalsIgnoreCase(name);
				} else {
					match = mask?schemaTagName.startsWith(name):schemaTagName.equals(name);
				}
				if(match) {
					tldElementType.setElementType(TldElementType.TAG);
					tldElements.add(tldElementType);
				}
				i++;
			}
		}

		getTldElementsByName().put(cacheKey, tldElements);

		return tldElements;
	}

	private class ResourceLoader implements Runnable {

		private KbTldResource resource;
		private Thread thread;
		private boolean loaded = false;

		public ResourceLoader(KbTldResource resource) {
			this.resource = resource;
		}

		public void start() {
			thread = new Thread(this);
			thread.setDaemon(false);
			thread.start();
		}

		public void run() {
			loadingResources.put(resource, resource);
			String uniqFileName = "schema";

			Document document = KbTldConvertor.getInstance().convertToSchema(resource);
//			KbPlugin.log("document: " + KbTldConvertor.getInstance().serialize(document.getDocumentElement()));
			if(document==null) {
				// Can't get document from resource
				// cancel loading
				loadingResources.remove(resource);
				return;
			}

			File schemaFolder = new File(schemaLocation);

			schemaFolder.mkdirs();
			File schemaFile = new File(schemaLocation + "/" + uniqFileName + ".xml");
			int i=1;
			while(schemaFile.exists()) {
				schemaFile = new File(schemaLocation + "/" + uniqFileName + i++ + ".xml");
			}

			try {
				// Serialize TLD schema...
				Element element = document.getDocumentElement();
				StringWriter sw = new StringWriter();
				XMLSerializer ser = new XMLSerializer(sw, createOutputFormat());

				ser.asDOMSerializer();
				ser.serialize(element);
				sw.close();

				BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(schemaFile));
				os.write(sw.toString().getBytes());
				os.flush();
				os.close();

//				TransformerFactory.newInstance().newTransformer().transform(new DOMSource(document), new StreamResult(schemaFile));
			} catch (Exception e) {
	        	KbPlugin.getPluginLog().logError(e);
				schemaFile.deleteOnExit();
				loadingResources.remove(resource);
				return;
			}
			resource.setSchemaLocation(schemaFile);
			resource.setCustomTld(true);
			registratedResources.put(resource, resource);
			activeDocuments.put(resource, document);
			loadingResources.remove(resource);
			loaded = true;
		}

		public boolean resourceHasLoaded() {
			return loaded;
		}

		public OutputFormat createOutputFormat() {
			OutputFormat format = new OutputFormat("xml", "UTF-8", true);
			format.setLineSeparator("\r\n");
			format.setIndent(2);
			return format;
		}
	}

	private static class TldElement {

		private KbTldResource resource;
		private Element element;
		private String prefix;
		private TldElementType elementType;

		public TldElement(KbTldResource resource, Element element, String prefix, TldElementType elementType) {
			this.resource = resource;
			this.element = element;
			this.prefix = prefix;
			this.elementType = elementType;
		}
		
		public Element getElement() {
			return element;
		}

		public KbTldResource getResource() {
			return resource;
		}

		public void setElement(Element element) {
			this.element = element;
		}

		public void setResource(KbTldResource resource) {
			this.resource = resource;
		}

		public String getPrefix() {
			return prefix;
		}

		public void setPrefix(String string) {
			prefix = string;
		}

		public TldElementType getElementType() {
			return elementType;
		}

		public boolean isPrefix() {
			return elementType == TldElementType.PREFIX;
		}

		public boolean isTag() {
			return elementType == TldElementType.TAG;
		}

		public boolean isAttribute() {
			return elementType == TldElementType.ATTRIBUTE;
		}

		public boolean isAttributeValue() {
			return elementType == TldElementType.ATTRIBUTE_VALUE;
		}

		public void setElementType(TldElementType type) {
			elementType = type;
		}

		public String toString() {
			StringBuffer result = new StringBuffer();
			result.append("Element: [");
			result.append(element);
			result.append("]; Type: [");
			result.append(elementType);
			result.append("]; Resource: [");
			result.append(resource);
			result.append("]; Prefix: [");
			result.append(prefix);
			result.append("]");

			return result.toString();
		}
	}

	private static class TldElementType {

		private final String type;

		private TldElementType(String type) {
			this.type = type;
		}

		public String toString() {
			return type;
		}

		public static final TldElementType PREFIX = new TldElementType("Prefix");
		public static final TldElementType TAG = new TldElementType("Tag");
		public static final TldElementType ATTRIBUTE = new TldElementType("Attribute");
		public static final TldElementType ATTRIBUTE_VALUE = new TldElementType("Attribute value");
	}
}