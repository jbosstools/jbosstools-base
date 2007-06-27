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
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author eskimo
 */
public class KbDtdStore implements KbStore {

	public static final String XML_DECLARATION = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

	private String schemaLocation;	
	private Vector<Document> activeDocuments;
	private Vector<KbDtdResource> registratedResources;
	private Vector<KbDtdResource> downloadingResources;

	private static final KbDtdStore INSTANCE = new KbDtdStore();

	private KbDtdStore() {
		if(KbPlugin.isDebugEnabled()) {		
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore()");
		}
		schemaLocation = KbConfigurationFactory.getInstance().getDefaultConfiguration().getDtdSchemaPath();

		activeDocuments = new Vector<Document>();
		registratedResources = new Vector<KbDtdResource>();
		downloadingResources = new Vector<KbDtdResource>();

		loadRegistratedResources();
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore()");
		}
	}

	/**
	 * 
	 * @return
	 */
	public static KbDtdStore getInstance() {
		return INSTANCE;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbStore#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor queryTagInformation(KbQuery query) {
//		String strQuery = query.getQuery();
		String lastTag = query.getLastTag();
		String queryForLastTag = query.getFullQueryForLastTag();
		if(queryForLastTag==null) {
			return null;
		}

		InerDtdQuery inerDtdQuery = getInerDtdQery(new KbQuery(queryForLastTag, query.getResources()));
		ArrayList elementTypes = inerDtdQuery.getElementTypes();

		return KbSchemaUtil.getTagInformationFromElementTypes(elementTypes, lastTag, new HashSet());
	}

	public AttributeDescriptor queryAttributeInformation(KbQuery query) {
		// TODO
		throw new RuntimeException("This method is not implemented yet.");
	}

	/**
	 * @see org.jboss.tools.common.kb.KbStore#queryProposal(org.jboss.tools.common.kb.KbQuery)
	 */
	public Collection<KbProposal> queryProposal(KbQuery query) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.queryProposal(KbQuery query)");
			KbPlugin.getPluginLog().logInfo("    query = " + query);
			KbPlugin.getPluginLog().logInfo("    registratedResources = " + registratedResources);
		}

		String strQuery = query.getQuery();

		if((strQuery.equals(KbQuery.XML_DECLARATION_QUERY))) {
			KbProposal proposal = new KbProposal();
			proposal.setLabel(XML_DECLARATION);
			proposal.setReplacementString(XML_DECLARATION);
			proposal.setContextInfo(null);
			ArrayList<KbProposal> proposals = new ArrayList<KbProposal>();
			proposals.add(proposal);
			return proposals;
		}

		ArrayList<KbProposal> proposals = queryTagProposal(getInerDtdQery(query));
		KbProposal endTag = getEndTagProposal(query);
		if(endTag != null) {
			proposals.add(endTag);
		}

		Collections.sort(proposals);

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("proposals size = " + proposals.size());
		}
		return proposals;
	}

	private InerDtdQuery getInerDtdQery(KbQuery query) {
		String strQuery = query.getQuery();

		Collection needResources = query.getResources();
		activateResources(needResources);

		int firstTagSeparator = strQuery.indexOf(KbQuery.TAG_SEPARATOR);
		if(firstTagSeparator == -1) {
//			KbPlugin.log("ERROR: Bad query: " + strQuery + ". Query must starts with \"" + KbQuery.TAG_SEPARATOR + "\" or \"" + KbQuery.XML_DECLARATION_QUERY + "\"");
			throw new RuntimeException("Bad query: " + strQuery + ". Query must starts with \"" + KbQuery.TAG_SEPARATOR + "\" or \"" + KbQuery.XML_DECLARATION_QUERY + "\"");
		}

		ArrayList<Element> elementTypes = new ArrayList<Element>();
		String tagQuery = null;
		int lastTagSeparator = strQuery.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(lastTagSeparator > firstTagSeparator) {
			String axis = strQuery.substring(firstTagSeparator + KbQuery.TAG_SEPARATOR.length(), lastTagSeparator);
			StringTokenizer tags = new StringTokenizer(axis, KbQuery.TAG_SEPARATOR);
			if(!tags.hasMoreTokens()) {
				String errorMessage = "ERROR: Bad query: " + strQuery + "\n" + 
									  "       Possible query formats: \"" + KbQuery.TAG_SEPARATOR +"rootTagName" + KbQuery.TAG_SEPARATOR + "childTagName1" + KbQuery.TAG_SEPARATOR + "..." + KbQuery.TAG_SEPARATOR + "childTagNameN" + KbQuery.ATTRIBUTE_SEPARATOR + "attributeName" + KbQuery.ENUMERATION_SEPARATOR + "attributeValue\"\n" +
									  "       or \"" + KbQuery.XML_DECLARATION_QUERY + "\"";
//				KbPlugin.log(errorMessage);
				throw new RuntimeException(errorMessage);
			}
			elementTypes = getChildElementTypes(needResources, tags);
			if(lastTagSeparator + KbQuery.TAG_SEPARATOR.length() < strQuery.length()) {
				tagQuery = strQuery.substring(lastTagSeparator);
			} else {
				tagQuery = KbQuery.TAG_SEPARATOR;
			}
		} else {
			Iterator iterator = needResources.iterator();
			while(iterator.hasNext()) {
				KbDtdResource resource = (KbDtdResource)iterator.next();
				Element rootElementType = getRootElementType(resource);
				if(rootElementType!=null) {
					elementTypes.add(rootElementType);
				}
				if(KbPlugin.isDebugEnabled()) {
					KbPlugin.getPluginLog().logInfo("    Root Element: " + rootElementType);
				}
//				KbPlugin.log("RESOURCE: " + resource);
/*
				Document document = getActiveDocument(resource);
				if(document != null) {
					elementTypes.add(getRootElementType(document));
				}
*/
			}
			tagQuery = strQuery;
		}

		InerDtdQuery inerDtdQuery = new InerDtdQuery(elementTypes, tagQuery);
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("    Iner DTD Query = " + inerDtdQuery);
		}

		return inerDtdQuery;
	}

	private KbProposal getEndTagProposal(KbQuery kbQuery) {
		String query = kbQuery.getQuery();
		String mask = "";
		int lastSeparator = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if((lastSeparator!=-1)&&(lastSeparator + KbQuery.TAG_SEPARATOR.length()<query.length())) {
			mask = query.substring(lastSeparator + KbQuery.TAG_SEPARATOR.length());
		}

		String lastTag = kbQuery.getLastTag();
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("last tag - " + lastTag);
			KbPlugin.getPluginLog().logInfo("mask - " + mask);
		}
		if((lastTag != null)&&(((mask.indexOf(KbQuery.DONT_FILTER_END_TAG_CHAR)!=-1)&&(KbQuery.DONT_FILTER_END_TAG_CHAR + lastTag).startsWith(mask))||(mask.equals("")))) {
			KbProposal proposal = new KbProposal();
			proposal.setLabel("/" + lastTag);
			proposal.setReplacementString("/" + lastTag);
			proposal.setIcon(KbIcon.XML_TAG);
			return proposal; 
		}

		return null;
	}
/*
	private ArrayList getEndTagProposal(ArrayList elementTypes) {
		ArrayList proposals = new ArrayList();
		for(Iterator iter = elementTypes.iterator(); iter.hasNext();) {
			Element elementType = (Element)iter.next();
			KbProposal proposal = new KbProposal();
			String name = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			proposal.setLabel("/" + name);
			proposal.setReplacementString("/" + name);
			proposal.setIcon(KbIcon.XML_TAG);

			proposals.add(proposal);
		}
		return proposals;
	}
*/

	public void registerResource(KbResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.registerResource(KbResource resource)");
			KbPlugin.getPluginLog().logInfo("    resource = " + resource);
		}

		KbDtdResource dtdResource = null;
		if(resource instanceof KbDtdResource) {
			dtdResource = (KbDtdResource)resource;
		} else {
			throw new IllegalArgumentException("KbDtdStore.registerResource(KbResource resource): resource must be instance of KbDtdResource");
		}

		if(checkActiveDocument(dtdResource)) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    resource already is activated");
				KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.registerResource()");
			}
			return;
		} else if(checkDownloadingResource(dtdResource)) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    resource is downloading");
				KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.registerResource()");
			}
			return;
		} else if(checkRegistratedResource(dtdResource)) {
			activateResource(dtdResource);
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    resource already is registred");
				KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.registerResource()");
			}
			return;
		}

		if((dtdResource.getDtdLocation()==null)&&(!KbConfigurationFactory.getInstance().getDefaultConfiguration().isAllowDownload())) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    Unknown resource but is not allow download.");
				KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.registerResource()");
			}
			return;
		}
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("    Unknown resource. Start downloading resource...");
		}

		new ResourceDownloader(dtdResource).start();

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.registerResource()");
		}
	}

	/**
	 * 
	 * @param resource
	 */
	public synchronized void reregisterModifiededResource(KbDtdResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.reregisterModifiededResource(KbDtdResource resource)");
			KbPlugin.getPluginLog().logInfo("    resource = " + resource);
		}
		KbDtdResource regResource = getRegistratedResource(resource);
		if((regResource!=null)&&regResource.isModified()) {
			unregisterResource(resource);
			registerResource(resource);
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    resource hase been reregistrated");
			}
		}
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.reregisterModifiededResource(KbDtdResource resource)");
		}
	}

	/*
	 * @see org.jboss.tools.common.kb.KbStore#unregisterResource(org.jboss.tools.common.kb.KbResource)
	 */
	public void unregisterResource(KbResource resource) {
		KbDtdResource dtdResource = (KbDtdResource)resource;
		KbDtdResource regResource = getRegistratedResource(dtdResource);
		if(regResource!=null) {
			registratedResources.remove(regResource);
			Document document = getActiveDocument(dtdResource);
			if(document!=null) {
				activeDocuments.remove(document);
			}
			File schema = regResource.getSchemaLocation();
			if(schema!=null) {
				schema.delete();
			}
		} else if(checkDownloadingResource(dtdResource)) {
//			TODO Stop downloading and registration resource
		}
	}

	private ArrayList<Element> getChildElementTypes(Collection resources, StringTokenizer tagNames) {
//		KbPlugin.log("--> getChildElementTypes(Collection resources, StringTokenizer tagNames)");
//		KbPlugin.log("    resources size = " + resources.size());
//		KbPlugin.log("    tagNames  = " + tagNames);

		String tagName = tagNames.nextToken();
//		KbPlugin.log("    tagName  = " + tagName);

		ArrayList<Element> childElements = getChildElements(resources, tagName, true);

//		KbPlugin.log("    childElements size = " +  childElements.size());

		while(tagNames.hasMoreElements()&&childElements.size()>0) {
			tagName = tagNames.nextToken();
//			KbPlugin.log("    tagName  = " + tagName);
			Iterator iterator = childElements.iterator();
			childElements = new ArrayList<Element>();
			while(iterator.hasNext()) {
				Element element = (Element)iterator.next();
				if(element.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE).equals(tagName)) {
//					KbPlugin.log("   add childElements size = " +  childElements.size());
					childElements = addCollection(childElements, getChildElements(resources, tagName, false));
				}
			}
		}

//		KbPlugin.log("    childElements size = " +  childElements.size());

		ArrayList<Element> childElementTypes = new ArrayList<Element>();
		for(int i=0; i<childElements.size(); i++) {
			Element elementType = KbSchemaUtil.getElementTypeByElement((Element)childElements.get(i));
			if(elementType!=null) {
				childElementTypes.add(elementType);
			}
		}

//		KbPlugin.log("<-- getChildElementTypes(Collection resources, StringTokenizer tagNames)");
//		KbPlugin.log("    childElementTypes size = " +  childElementTypes.size());
		return childElementTypes; 
	}

/*
	private Element getRootElementType(Document document) {
		String rootElementName = document.getDocumentElement().getAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE);

//		KbPlugin.log("rootElementName = " + rootElementName);

		NodeList nodeList = document.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int j=0; j<nodeList.getLength(); j++) {
			Node child = nodeList.item(j); 
			if(child instanceof Element) {
				Element element = (Element)child;
				if(rootElementName.equals(element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE))) {
//					KbPlugin.log("rootElementName = " + element.getAttribute("name"));
					return element;
				}
			}
		}

//		KbPlugin.log("ERROR: Can't find ElementType for tagName (name=\"" + rootElementName + "\").");
		return null;
	}
*/

    private Element getRootElementType(KbDtdResource resource) {
		Document document = getActiveDocument(resource);
		if(document != null) {
			String rootElementName = resource.getRootElement();
			if(rootElementName == null) {
				rootElementName = document.getDocumentElement().getAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE);
			}
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("    Root element name: " + rootElementName);
			}
			return KbSchemaUtil.getElementTypeByName(document, rootElementName, false);
		}
//		KbPlugin.log("DOCUMENT IS null!");
		return null;
    }

	private ArrayList<Element> addCollection(Collection<Element> col1, Collection<Element> col2) {
		ArrayList<Element> arrayList = new ArrayList<Element>();
		Iterator<Element> iterator = col1.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		iterator = col2.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		return arrayList;
	}

	private ArrayList<Element> getChildElements(Collection resources, String elementTypeName, boolean rootTag) {
//		KbPlugin.log("--> getChildElements(Collection resources, String elementTypeName, boolean rootTag)");
//		KbPlugin.log("    resources size = " + resources.size());
//		KbPlugin.log("    elementTypeName  = " + elementTypeName);

		ArrayList<Element> elements = new ArrayList<Element>();

		ArrayList<Document> documents = new ArrayList<Document>();
		Iterator iterator = resources.iterator();
		while(iterator.hasNext()) {
			KbDtdResource resource = (KbDtdResource)iterator.next();
			Document document = getActiveDocument(resource);
/*
			if((document != null)&&(!rootTag || KbSchemaUtil.checkRootElement(document, elementTypeName))) {
				documents.add(document);
			}
*/

			String rootElementName = resource.getRootElement();
			if(rootElementName == null) {
				rootElementName = document.getDocumentElement().getAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE);
			}

			if((document != null)&&(!rootTag || rootElementName.equals(elementTypeName))) {
				documents.add(document);
			}
		}

		for(int i=0; i< documents.size(); i++) {
			Document document = (Document)documents.get(i);
			NodeList elementTypes = document.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
			for(int j=0; j<elementTypes.getLength(); j++) {
				Node child = elementTypes.item(j); 
				if(child instanceof Element) {
					Element elementType = (Element)child;
//					KbPlugin.log("Element type: " + elementType);
					if(elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE).equals(elementTypeName)) {
						NodeList childElements = elementType.getElementsByTagName(SchemaNodeFactory.ELEMENT_NODE);
						for(int g=0; g<childElements.getLength(); g++) {
							Node childElement = childElements.item(g);
							if(childElement instanceof Element) {
//								KbPlugin.log("Add Element: " + ((Element)childElement).getAttribute("type"));
								elements.add((Element)childElement);
							}
						} 
					}
				}
			}
		}

//		KbPlugin.log("<-- getChildElements(Collection resources, String elementTypeName, boolean rootTag)");
//		KbPlugin.log("    element size = " + elements.size());
		return elements;
	}

	private ArrayList<KbProposal> queryTagProposal(InerDtdQuery query) {
		return queryTagProposal(query.getElementTypes(), query.getTagQuery());
	}

	private ArrayList<KbProposal> queryTagProposal(ArrayList elementTypes, String query) {
//		KbPlugin.log("-->  queryTagProposal(ArrayList elementTypes, String query)");
//		KbPlugin.log("     Query = " + query);
//		KbPlugin.log("     elementTypes size = " + elementTypes.size());

		if((!query.startsWith(KbQuery.TAG_SEPARATOR))||
		   (query.startsWith(KbQuery.TAG_SEPARATOR + KbQuery.ATTRIBUTE_SEPARATOR))) {
			return new ArrayList<KbProposal>();
		} else if(query.length() == KbQuery.TAG_SEPARATOR.length()) {
			return getTags(elementTypes, "");
		}

		int startAttributeName = query.indexOf(KbQuery.ATTRIBUTE_SEPARATOR);
		if(startAttributeName < 0){
			return getTags(elementTypes, query.substring(KbQuery.TAG_SEPARATOR.length()));
		}

		String tagName = query.substring(KbQuery.TAG_SEPARATOR.length(), startAttributeName);
		startAttributeName+=KbQuery.ATTRIBUTE_SEPARATOR.length();
		if(startAttributeName == query.length()) {
			return getAttributes(elementTypes, tagName, "");
		}

		int startAttributeValue = query.indexOf(KbQuery.ENUMERATION_SEPARATOR);
		if(startAttributeValue < 0) {
			return getAttributes(elementTypes, tagName, query.substring(startAttributeName));
		}

		String attributeName = query.substring(startAttributeName, startAttributeValue);
		startAttributeValue+=KbQuery.ENUMERATION_SEPARATOR.length();
		if(startAttributeValue == query.length()) {
			return getEnumeration(elementTypes, tagName, attributeName, "");
		}

		return getEnumeration(elementTypes, tagName, attributeName, KbQuery.decode(query.substring(startAttributeValue)));
	}

	private synchronized boolean checkActiveDocument(KbDtdResource resource) {
		return getActiveDocument(resource) != null;
	}

	private synchronized boolean checkDownloadingResource(KbDtdResource resource) {
		synchronized (downloadingResources) {
			for(int i=0; i<downloadingResources.size(); i++) {
				if(((KbDtdResource)downloadingResources.get(i)).equals(resource)) {
					return true;
				}
/*				String downloadingUri = ((KbDtdResource)downloadingResources.get(i)).getUri();
				URL downloadingUrl = ((KbDtdResource)downloadingResources.get(i)).getUrl();
				if(downloadingUri.equals(resource.getUri())) {
					// Is downloading this resource.
					return true;
				} else if(downloadingUrl.equals(resource.getUrl())) {
					// Is downloading this resource.
					return true;
				}
*/
			}
		}
		// Isn't downloading this resource.
		return false;
	}

	private synchronized boolean checkRegistratedResource(KbDtdResource resource) {
		return getRegistratedResource(resource) != null;
	}

	private synchronized KbDtdResource getRegistratedResource(KbDtdResource dtdResource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.getRegistratedResource(KbDtdResource dtdResource)");
			KbPlugin.getPluginLog().logInfo("    dtdResource = " + dtdResource);
		}

		synchronized (registratedResources) {
			for(int i=0; i<registratedResources.size(); i++){
				KbDtdResource resource = (KbDtdResource)registratedResources.get(i);
				if(resource.equals(dtdResource)) {
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.getPluginLog().logInfo("    Found resource");
						KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.getRegistratedResource(KbDtdResource dtdResource)");
						KbPlugin.getPluginLog().logInfo("    return resource = " + resource);
					}
					return resource;
				}
/*				String registratedUri = resource.getUri();
				URL registratedUrl = resource.getUrl();
	//			KbPlugin.log("reg-d uri=" + registratedUri);
	//			KbPlugin.log("reg uri=" + uri);
				if(registratedUri.equals(dtdResource.getUri())) {
					// Registrated resource.
					return resource;
				} else if(registratedUrl.equals(dtdResource.getUrl())) {
					// Registrated resource.
					return resource;
				}
*/
			}
		}
		// Non-registrated resource.
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("    Did not find resource");
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.getRegistratedResource(KbDtdResource dtdResource)");
			KbPlugin.getPluginLog().logInfo("    return resource = null");
		}
		return null;
	}

	private synchronized Document getActiveDocument(KbDtdResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.getActiveDocument(KbDtdResource resource)");
			KbPlugin.getPluginLog().logInfo("    resource = " + resource);
		}

		synchronized (activeDocuments) {
			for(int i=0; i<activeDocuments.size(); i++){
				Document activeDocument = (Document)activeDocuments.get(i);
				String activeDocumentsUri = activeDocument.getDocumentElement().getAttribute(SchemaNodeFactory.URI_ATTRIBUTE);
				String activeDocumentsUrl = activeDocument.getDocumentElement().getAttribute(SchemaNodeFactory.URL_ATTRIBUTE);
				if((activeDocumentsUri.equals(resource.getUri()))||(activeDocumentsUrl.equals(resource.getId()))) {
					// Active resource.
					if(KbPlugin.isDebugEnabled()) {
						KbPlugin.getPluginLog().logInfo("    Found document");
						KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.getActiveDocument(KbDtdResource resource)");
						KbPlugin.getPluginLog().logInfo("    return document = " + activeDocument);
					}
					return activeDocument;
				}
			}
		}
		// Non-active resource.
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("    Did not find document");
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.getActiveDocument(KbDtdResource resource)");
			KbPlugin.getPluginLog().logInfo("    return document = null");
		}
		return null;
	}

	private synchronized void activateResource(KbDtdResource resource) {
		if(checkActiveDocument(resource)) {
			return;
		}

		KbDtdResource regResource = getRegistratedResource(resource);
		if(regResource == null) {
			return;
		}

		File schemaLocation = regResource.getSchemaLocation();
		if((schemaLocation == null)||(!schemaLocation.exists())) {
//			KbPlugin.log("ERROR: Schema (location: " + schemaLocation + ") for resource (" + regResource +") does not exist!");
			return;
		}

		Document document = null;
		try {
			document = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(schemaLocation);
		} catch (Exception e) {
			String message = "ERROR: Can't parse Schema (location: " + schemaLocation + ")";
			KbPlugin.getPluginLog().logError(message, e);
			return;
		}
		activeDocuments.add(document);
	}

	private synchronized void activateResources(Collection resources) {
		Iterator iterator = resources.iterator();
		while(iterator.hasNext()) {
			KbDtdResource resource = (KbDtdResource)iterator.next();
			activateResource(resource);
		}
	}

	public synchronized void activateAllResources() {
		synchronized (registratedResources) {		
			for(int i=0; i<registratedResources.size(); i++) {
				KbDtdResource resource = (KbDtdResource)registratedResources.get(i);
				activateResource(resource);
			}
		}
	}
/*
	private synchronized void deactivateResource(KbDtdResource resource) {
		Document document = getActiveDocument(resource);
		if(document!=null) {
			activeDocuments.remove(document);
		}
	}
*/
	public synchronized void deactivateAllResources() {
		for(int i=0; i<activeDocuments.size(); i++) {
			activeDocuments.clear();
		}
	}

	private void loadRegistratedResources() {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("--> KbDtdStore.loadRegistratedResources()");
		}

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
				if(KbPlugin.isDebugEnabled()) {
					String message = "WARNING: Can't parse Schema (location: " + schemas[i] + ")";
					KbPlugin.getPluginLog().logError(message, e);
				}
				continue;
			}
			String uri = document.getDocumentElement().getAttribute(SchemaNodeFactory.URI_ATTRIBUTE);
			String url = document.getDocumentElement().getAttribute(SchemaNodeFactory.URL_ATTRIBUTE);
			KbDtdResource resource = null;
			try {
				resource = new KbDtdResource(uri, new URL(url), null);
			} catch (Exception e) {
				if(KbPlugin.isDebugEnabled()) {
					String message = "ERROR: can't create java.net.URL for url=\"" + url + "\" from schema (location=" + schemas[i] + ")";
					KbPlugin.getPluginLog().logError(message);
				}
				continue;
			}
			resource.setSchemaLocation(schemas[i]);
			registratedResources.add(resource);
		}

		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.getPluginLog().logInfo("<-- KbDtdStore.loadRegistratedResources()");
		}
	}

	private ArrayList<KbProposal> getTags(Collection elementTypes, String tagMask) {
//		KbPlugin.log("--> getTags(Collection elementTypes, String tagMask)");
//		KbPlugin.log("    elementTypes size = " + elementTypes.size());
//		KbPlugin.log("    tagMask = " + tagMask);
//		KbPlugin.log("<-- getTags()");
		return getTagProposal(getElementTypesByName(elementTypes, tagMask, true));
	}

	private ArrayList<Element> getElementTypesByName(Collection elementTypes, String name, boolean mask) {
//		KbPlugin.log("--> getElementTypesByName(Collection elementTypes, String name, boolean mask)");
//		KbPlugin.log("    elementTypes size = " + elementTypes.size());
//		KbPlugin.log("    name = " + name);
//		KbPlugin.log("    mask = " + mask);

		ArrayList<Element> elements = new ArrayList<Element>();

		Iterator iterator = elementTypes.iterator();
		while(iterator.hasNext()) {
			Element elementType = (Element)iterator.next();
			String schemaTagName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			boolean match = mask?schemaTagName.startsWith(name):schemaTagName.equals(name);
			if(match) {
//				KbPlugin.log("    add element: " + elementType);
				elements.add(elementType);
			}
		}

//		KbPlugin.log("<-- getElementTypesByName()");
//		KbPlugin.log("    elements Size = " + elements.size());

		return elements;
	}

	private ArrayList<KbProposal> getAttributes(Collection elementTypes, String tagName, String attributeMask) {
//		KbPlugin.log("--> getAttributes(String prefix, String tagName, String attributeMask)");
//		KbPlugin.log("    prefix = " + prefix);
//		KbPlugin.log("    tagName = " + tagName);
//		KbPlugin.log("    attributeMask = " + attributeMask);
//		KbPlugin.log("<-- getAttributes()");
		return getAttributeProposal(getAttributeTypesByName(elementTypes, tagName, attributeMask, true));
	}

	private ArrayList<Element> getAttributeTypesByName(Collection elementTypes, String tagName, String attributeName, boolean mask) {
		ArrayList<Element> attributes = new ArrayList<Element>();

		ArrayList elements = getElementTypesByName(elementTypes, tagName, false);
		if(!elements.isEmpty()) {
			Element element = (Element)elements.get(0);

			ArrayList attributeTypes = KbSchemaUtil.getAttributeTypes(element);
			for(int i=0; i<attributeTypes.size(); i++) {
				Element attributeType = (Element)attributeTypes.get(i);

				String attributeTypeName = attributeType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				boolean match = mask?attributeTypeName.startsWith(attributeName):attributeTypeName.equals(attributeName);
				if(match) {
					attributes.add(attributeType);
				}
			}
		}
		return attributes;
	}

	private ArrayList<KbProposal> getEnumeration(Collection elementTypes, String tagName, String attributeName, String valueMask) {
		ArrayList<KbProposal> enumeration = new ArrayList<KbProposal>();

		ArrayList attributeTypes = new ArrayList();
		attributeTypes = getAttributeTypesByName(elementTypes, tagName, attributeName, false);
		if(!attributeTypes.isEmpty()) {
			Element attributeType = (Element)attributeTypes.get(0);
			String type = attributeType.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);
			if(type.equals(SchemaNodeFactory.ENUMERATION_TYPE)) {
				String values = attributeType.getAttribute(SchemaNodeFactory.VALUES_ATTRIBUTE);
				StringTokenizer st = new StringTokenizer(values, SchemaNodeFactory.ENUMERATION_SEPARATOR);
				while(st.hasMoreElements()) {
					String value = (String)st.nextElement();
					if(value.startsWith(valueMask)) {
						KbProposal proposal = new KbProposal();
						proposal.setLabel(value);
						proposal.setReplacementString(value);
						proposal.setContextInfo(null);
						proposal.setIcon(KbIcon.ENUM_ITEM);

						enumeration.add(proposal);
					}
				}
			}
		}

		return enumeration;
	}

	private ArrayList<KbProposal> getTagProposal(ArrayList<Element> elements) {
		return getProposal(elements, true);
	}

	private ArrayList<KbProposal> getAttributeProposal(ArrayList elements) {
		return getProposal(elements, false);
	}

	private ArrayList<KbProposal> getProposal(ArrayList elements, boolean tag) {
//		KbPlugin.log("--> getProposal(ArrayList elements, boolean tag)");
//		KbPlugin.log("    elemnets size = " + elements.size());
//		KbPlugin.log("    tag = " + tag);

		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(int i=0; i<elements.size(); i++) {
			Element element = (Element)elements.get(i);
			String tagName = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);

			KbProposal proposal = new KbProposal();

			if(tag) {
				String endTag = SchemaNodeFactory.REFUSED_BODY_CONTENT_TYPE.equals(element.getAttribute(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE))?" /":"";

				String attributes = KbSchemaUtil.getRequaredAttributesAsString(element);
				proposal.setReplacementString(tagName + attributes + endTag);
				int position = proposal.getReplacementString().indexOf('"');
				if(position!=-1) {
					position ++;
				} else {
					position = proposal.getReplacementString().length();
				}
				proposal.setPosition(position);
				proposal.setLabel(tagName);
				proposal.setIcon(KbIcon.XML_TAG);
			} else {
				proposal.setReplacementString(tagName);
				if(KbSchemaUtil.checkRequaredAttribute(element)) {
					proposal.setIcon(KbIcon.XML_ATTRIBUTE);
				} else {
					proposal.setIcon(KbIcon.XML_ATTRIBUTE_OPTIONAL);
				}
				proposal.setLabel(tagName);
			}

			proposal.setContextInfo(KbSchemaUtil.getDescription(element));

//			KbPlugin.log("    add kbProposals:\n" + proposal.toString());
//			KbPlugin.log("    for element: " + element);
//			proposal.setContextInfo(proposal.getIcon().toString());

			kbProposals.add(proposal);
		}

//		KbPlugin.log("<-- getProposal()");
//		KbPlugin.log("    kbProposals size = " + kbProposals.size());

		return kbProposals;
	}

	private class ResourceDownloader implements Runnable {

		private KbDtdResource resource;
		private Thread thread;

		public ResourceDownloader(KbDtdResource resource) {
			this.resource = resource;
		}

		public void start() {
			thread = new Thread(this);
			thread.setDaemon(false);
			thread.start();
		}

		public void run() {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logInfo("downloading resource - " + resource);
			}
			downloadingResources.add(resource);
			String uniqFileName = "schema.xml";
			try {
				File tempFile = File.createTempFile("schema", ".xml");
				uniqFileName = tempFile.getName();
				tempFile.deleteOnExit();
			} catch(Exception e) {
				String message = "WARNING: Can't create temp file.";
				KbPlugin.getPluginLog().logError(message, e);
			}
			File schemaFolder = new File(schemaLocation);

			schemaFolder.mkdirs();
			File schemaFile = new File(schemaLocation + "/" + uniqFileName);
			int i=0;
			while(schemaFile.exists()) {
				schemaFile = new File(schemaLocation + "/" + i++ + uniqFileName);
			}

			Document document = KbDtdConvertor.getInstance().convertToSchema(resource);
			if(document == null) {
				if(KbPlugin.isDebugEnabled()) {
					KbPlugin.getPluginLog().logInfo("    Can't get document from resource.");
					KbPlugin.getPluginLog().logInfo("    cancel downloading.");
				}
				downloadingResources.remove(resource);
				return;
			}
			try {
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
				String message = "ERROR: Can't serialize DTD schema (" + schemaFile + ").";
				KbPlugin.getPluginLog().logError(message, e);
				schemaFile.deleteOnExit();
				downloadingResources.remove(resource);
				return;
			}
			resource.setSchemaLocation(schemaFile);
			registratedResources.add(resource);
			activeDocuments.add(document);
			downloadingResources.remove(resource);
		}

		public OutputFormat createOutputFormat() {
			OutputFormat format = new OutputFormat("xml", "UTF-8", true);
			format.setLineSeparator("\n");
			format.setIndent(2);
			return format;
		}
	}

	private class InerDtdQuery {

		private ArrayList elementTypes;
		private String tagQuery;

		public InerDtdQuery(ArrayList elementTypes, String tagQuery) {
			this.elementTypes = elementTypes;
			this.tagQuery = tagQuery;
		}

        /**
         * @return
         */
        public ArrayList getElementTypes() {
            return elementTypes;
        }

        /**
         * @return
         */
        public String getTagQuery() {
            return tagQuery;
        }

        /**
         * @param list
         */
        public void setElementTypes(ArrayList list) {
            elementTypes = list;
        }

        /**
         * @param string
         */
        public void setTagQuery(String string) {
            tagQuery = string;
        }

		public String toString() {
			return new StringBuffer().append("Element types: ").append(elementTypes).append("; Element types size: ").append(elementTypes==null?"null":""+elementTypes.size()).append("; tagQuery: ").append(tagQuery).toString();
		}
	}

}
