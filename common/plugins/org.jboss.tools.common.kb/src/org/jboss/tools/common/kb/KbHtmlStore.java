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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author eskimo
 */
public class KbHtmlStore implements KbStore {

	private boolean activatingHtmlSchema = false;

	private String htmlSchemaLocation;
	private Document htmlSchema;
//	private KbHtmlMap htmlMap;

	private static final KbHtmlStore INSTANCE = new KbHtmlStore();

	private static Map<String,TagDescriptor> tagInfoCache = new HashMap<String,TagDescriptor>(200);
	private static Map<String,Element> elementTypeCache = new HashMap<String,Element>(200);

	private KbHtmlStore() {
		htmlSchemaLocation = KbConfigurationFactory.getInstance().getDefaultConfiguration().getHtmlSchemaFilePath();
	}

	public static KbHtmlStore getInstance() {
		return INSTANCE;
	}

	public void clearCache() {
		tagInfoCache.clear();
		elementTypeCache.clear();
	}

	private static Map<String,TagDescriptor> getTagInfoCache() {
	    if(tagInfoCache.size()>200) {
	        tagInfoCache.clear();
	    }
	    return tagInfoCache;
	}

	private static Map<String,Element> getElementTypeCache() {
	    if(elementTypeCache.size()>200) {
	    	elementTypeCache.clear();
	    }
	    return elementTypeCache;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbStore#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor queryTagInformation(KbQuery query) {
		String tagName = query.getLastTagName();

		Object o = getTagInfoCache().get(tagName);
        if(o!=null) {
            return (TagDescriptor)o;
        }

        if(!activateHtmlSchema()) {
			return null;
		} 

		if(tagName==null) {
			return null;
		}

		Element elementType = getElementTypeByName(tagName);
		if(elementType==null) {
			return null;
		}
		TagDescriptor tagInfo = KbSchemaUtil.getTagInformationFromElementType(elementType);

		getTagInfoCache().put(tagName, tagInfo);
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

	public Collection<KbProposal> queryProposal(KbQuery query) {
		if(!activateHtmlSchema()) {
			return new ArrayList<KbProposal>();
		} 

		String strQuery = cleanQuery(query);
		if(strQuery == null) {
			String errorMessage = "ERROR: Bad query: \"" + query.getQuery() + "\". Query must starts with \"" + KbQuery.TAG_SEPARATOR + "\"";
//			KbPlugin.log(errorMessage);
			throw new RuntimeException(errorMessage);
		}

		int firstTagSeparator = strQuery.indexOf(KbQuery.TAG_SEPARATOR);

//		String tagQuery = null;
		int lastTagSeparator = strQuery.lastIndexOf(KbQuery.TAG_SEPARATOR);

		String firstTag = null;
		String tagMask = "";
		if(lastTagSeparator > firstTagSeparator) {
			firstTag = strQuery.substring(firstTagSeparator + KbQuery.TAG_SEPARATOR.length(), lastTagSeparator);
			if(lastTagSeparator + KbQuery.TAG_SEPARATOR.length() < strQuery.length()) {
				tagMask = strQuery.substring(lastTagSeparator + KbQuery.TAG_SEPARATOR.length());
			}
		} else {
			tagMask = strQuery.substring(firstTagSeparator + KbQuery.TAG_SEPARATOR.length());
		} 

		ArrayList<KbProposal> proposals = null;
		if(tagMask.indexOf(KbQuery.ATTRIBUTE_SEPARATOR)<0) {
			proposals = queryTagProposal(firstTag, tagMask);
			KbProposal endTag = getEndTagProposal(new KbQuery(strQuery));
			if(endTag!=null) {
				proposals.add(endTag);
			}
		} else {
			proposals = queryAttributeProposal(tagMask);
		}

		Collections.sort(proposals);

		return proposals;
	}

	private String cleanQuery(KbQuery query) {
		String strQuery = query.getQuery().toUpperCase();

		StringBuffer cleanedQuery = new StringBuffer();
		cleanedQuery.append(KbQuery.TAG_SEPARATOR);

		StringTokenizer tk = new StringTokenizer(strQuery, KbQuery.TAG_SEPARATOR, true);
		while(tk.hasMoreElements()) {
			String tag = tk.nextToken();
			if(tag.equals(KbQuery.TAG_SEPARATOR)) {
				continue;
			}
			if(tk.hasMoreElements()) {
				if(checkNonRefusedEndTag(tag)) {
					cleanedQuery.append(tag);
					cleanedQuery.append(KbQuery.TAG_SEPARATOR);
				}
			} else {
				cleanedQuery.append(tag);
			}
		}
		String result = getTwoLastTags(cleanedQuery.toString());
		return result;
	}

	private boolean checkNonRefusedEndTag(String tagName) {
		Element elementType = getElementTypeByName(tagName);
		if(elementType==null) {
			return true;
		}
		String endTagType = elementType.getAttribute(SchemaNodeFactory.END_TAG_ATTRIBUTE);
		if(SchemaNodeFactory.REFUSED_TAG_TYPE.equals(endTagType)) {
			return false;
		}

		return true;
	}

	private Element getElementTypeByName(String name) {
        Object o = getElementTypeCache().get(name);
        if(o!=null) {
            return (Element)o;
        }

		NodeList nodeList = htmlSchema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);

		for(int i=0; i<nodeList.getLength(); i++) {
			Element elementType = (Element)nodeList.item(i);
			String schemaTagName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			if(schemaTagName.equalsIgnoreCase(name)) {
				getElementTypeCache().put(name, elementType);
				return elementType;
			}
		}
		return null;
	}

	private String getTwoLastTags(String query) {
		int startLastTag = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(startLastTag<0) {
			return null;
		}
		String q = query.substring(0, startLastTag);
		int startNextToLastTag = q.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if(startNextToLastTag<0) {
			return query.substring(startLastTag);
		}

		return query.substring(startNextToLastTag);
	}

	private KbProposal getEndTagProposal(KbQuery kbQuery) {
		String query = kbQuery.getQuery();
		String mask = "";
		int lastSeparator = query.lastIndexOf(KbQuery.TAG_SEPARATOR);
		if((lastSeparator!=-1)&&(lastSeparator + KbQuery.TAG_SEPARATOR.length()<query.length())) {
			mask = query.substring(lastSeparator + KbQuery.TAG_SEPARATOR.length());
		}

		String lastTag = kbQuery.getLastTag();
		if((lastTag != null)&&(((mask.indexOf(KbQuery.DONT_FILTER_END_TAG_CHAR)!=-1)&&(KbQuery.DONT_FILTER_END_TAG_CHAR + lastTag).startsWith(mask))||(mask.equals("")))) {
			if(checkNonRefusedEndTag(lastTag)) {
				KbProposal proposal = new KbProposal();
				proposal.setLabel("/" + lastTag);
				proposal.setReplacementString("/" + lastTag);
				proposal.setIcon(KbIcon.HTML_TAG);
				return proposal;
			} 
		}

		return null;
	}

	public void registerResource(KbResource resource) {
	}

	public void unregisterResource(KbResource resource) {
	}

	private ArrayList<KbProposal> queryTagProposal(String firstTag, String tagMask) {
		ArrayList<KbProposal> returnProposals = new ArrayList<KbProposal>();

		if(firstTag!=null) {
			returnProposals = getTags(firstTag, tagMask);
		} else {
			returnProposals = getTags(htmlSchema.getDocumentElement().getAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE), tagMask);
		}

		return returnProposals;
	}

	private ArrayList<KbProposal> getTags(String firstTag, String tagMask) {
		return getTagProposal(getHtmlElementTypesByName(firstTag, tagMask));
	}

	private ArrayList<Element> getHtmlElementTypesByName(String firstTag, String tagMask) {

		ArrayList<Element> htmlElementTypes = new ArrayList<Element>();

		Element firstElementType = KbSchemaUtil.getElementTypeByName(this.htmlSchema, firstTag, true);
		if(firstElementType == null) {
			return htmlElementTypes;
		}

		NodeList nodeList = firstElementType.getElementsByTagName(SchemaNodeFactory.ELEMENT_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if(node instanceof Element) {
				Element element = (Element)node;
				String elementName = element.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);

				if(elementName.startsWith(tagMask)) {
					Element elementType = KbSchemaUtil.getElementTypeByElement(element);
					if(elementType!=null) {
						htmlElementTypes.add(elementType);
					}
				}
			}
		}

		return htmlElementTypes;
	}

	private static ArrayList<KbProposal> getTagProposal(Collection<Element> elementTypes) {
		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(Element elementType : elementTypes) {
			String label = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);

			KbProposal proposal = new KbProposal();
			proposal.setLabel(label);
			proposal.setContextInfo(KbSchemaUtil.getDescription(elementType));

			String attributes = KbSchemaUtil.getRequaredAttributesAsString(elementType);
			proposal.setReplacementString(label + attributes);
			int position = proposal.getReplacementString().indexOf('"');
			if(position!=-1) {
				position ++;
			} else {
				position = proposal.getReplacementString().length();
			}
			proposal.setPosition(position);
			proposal.setIcon(KbIcon.HTML_TAG);

			kbProposals.add(proposal);
		}

		return kbProposals;
	}

	private ArrayList<KbProposal> queryAttributeProposal(String tagMask) {
		int startAttributeName = tagMask.indexOf(KbQuery.ATTRIBUTE_SEPARATOR);

		String tagName = tagMask.substring(0, startAttributeName);
		startAttributeName+=KbQuery.ATTRIBUTE_SEPARATOR.length();
		if(startAttributeName == tagMask.length()) {
			return getAttributes(tagName, "");
		}

		int startAttributeValue = tagMask.indexOf(KbQuery.ENUMERATION_SEPARATOR);
		if(startAttributeValue < 0) {
			return getAttributes(tagName, tagMask.substring(startAttributeName));
		}

		String attributeName = tagMask.substring(startAttributeName, startAttributeValue);
		startAttributeValue+=KbQuery.ENUMERATION_SEPARATOR.length();
		if(startAttributeValue == tagMask.length()) {
			return getEnumeration(tagName, attributeName, "");
		}

		return getEnumeration(tagName, attributeName, KbQuery.decode(tagMask.substring(startAttributeValue)));
	}

	private synchronized boolean activateHtmlSchema() {
		if(htmlSchemaIsActivating()) {
			return false;
		}

		if(htmlSchema != null) {
			return true;
		}

		File schemaLocation = new File(htmlSchemaLocation);
		if((schemaLocation == null)||(!schemaLocation.exists())) {
			return false;
		}

		try {
			htmlSchema = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(new File(htmlSchemaLocation));
		} catch (Exception e) {
			KbPlugin.log(e);
			return false;
		}
		return true;
	}

	private synchronized boolean htmlSchemaIsActivating() {
		return activatingHtmlSchema;
	}
/*
	private synchronized void enableHtmlSchemaActivating(boolean status) {
		activatingHtmlSchema = status;
	}

	private synchronized void deactivateHtmlSchema() {
		htmlSchema = null;
//		htmlMap = null;
	}
*/
	private ArrayList<KbProposal> getAttributes(String tagName, String attributeMask) {
		return getAttributeProposal(getAttributeTypesByName(tagName, attributeMask, true));
	}

	private ArrayList<Element> getAttributeTypesByName(String tagName, String attributeName, boolean mask) {
		ArrayList<Element> attributes = new ArrayList<Element>();

		Element element = getElementTypeByName(tagName);

		if(element==null) {
			return attributes;
		}
		ArrayList<Element> attributeTypes = KbSchemaUtil.getAttributeTypes(element);
		for(Element attributeType : attributeTypes) {
			String attributeTypeName = attributeType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE).toUpperCase();

			boolean match = mask?attributeTypeName.toUpperCase().startsWith(attributeName):attributeTypeName.equalsIgnoreCase(attributeName);
			if(match) {
				attributes.add(attributeType);
			}
		}

		return attributes;
	}

	private ArrayList<KbProposal> getEnumeration(String tagName, String attributeName, String valueMask) {
		ArrayList<KbProposal> enumeration = new ArrayList<KbProposal>();

		ArrayList<Element> attributeTypes = getAttributeTypesByName(tagName, attributeName, false);
		if(!attributeTypes.isEmpty()) {
			Element attributeType = attributeTypes.get(0);
			String type = attributeType.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);
			if(type.equals(SchemaNodeFactory.ENUMERATION_TYPE)) {
				String values = attributeType.getAttribute(SchemaNodeFactory.VALUES_ATTRIBUTE);
				StringTokenizer st = new StringTokenizer(values, SchemaNodeFactory.ENUMERATION_SEPARATOR);
				while(st.hasMoreElements()) {
					String value = (String)st.nextElement();
					if(value.toUpperCase().startsWith(valueMask)) {
						KbProposal proposal = new KbProposal();
						proposal.setLabel(value);
						proposal.setReplacementString(value);
						proposal.setContextInfo(null);
						proposal.setIcon(KbIcon.ENUM_ITEM);
						proposal.setPosition(value.length());

						enumeration.add(proposal);
					}
				}
			}
		}

		return enumeration;
	}

	private ArrayList<KbProposal> getAttributeProposal(ArrayList<Element> elements) {
		return getProposal(elements, false);
	}

	private ArrayList<KbProposal> getProposal(Collection<Element> elements, boolean tag) {
		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(Element element : elements) {
			String label = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);

			KbProposal proposal = new KbProposal();
			proposal.setLabel(label);
			proposal.setContextInfo(KbSchemaUtil.getDescription(element));

			if(tag) {
				String attributes = KbSchemaUtil.getRequaredAttributesAsString(element);
				proposal.setReplacementString(label + attributes);
				int position = proposal.getReplacementString().indexOf('"');
				if(position!=-1) {
					position ++;
				} else {
					position = proposal.getReplacementString().length();
				}
				proposal.setPosition(position);
				proposal.setIcon(KbIcon.HTML_TAG);
			} else {
				proposal.setReplacementString(label);
				if(KbSchemaUtil.checkRequaredAttribute(element)) {
					proposal.setIcon(KbIcon.HTML_ATTRIBUTE);
				} else {
					proposal.setIcon(KbIcon.HTML_ATTRIBUTE_OPTIONAL);
				}
			}

			kbProposals.add(proposal);
		}

		return kbProposals;
	}
}