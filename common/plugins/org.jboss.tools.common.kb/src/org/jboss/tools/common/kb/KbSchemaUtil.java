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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * @author igels
 */
public class KbSchemaUtil {

	/**
	 * 
	 * @param element
	 * @return
	 */
	public static Element getElementTypeByElement(Element element) {
		String elementName = element.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);
		Document document = element.getOwnerDocument();
		NodeList elementTypes = document.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int j=0; j<elementTypes.getLength(); j++) {
			Node child = elementTypes.item(j); 
			if(child instanceof Element) {
				Element elementType = (Element)child;
				if(elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE).equalsIgnoreCase(elementName)) {
//					KbPlugin.log("    add element: " + element);
					return elementType;
				}
			}
		}
//		KbPlugin.log("ERROR: Can't find ElementType for element (name=\"" + elementName + "\").");
		return null;
	}

	/**
	 * 
	 * @param schema
	 * @param elementTypeName
	 * @param ignoreCase
	 * @return
	 */
	public static Element getElementTypeByName(Document schema, String elementTypeName, boolean ignoreCase) {
		NodeList nodeList = schema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if(node instanceof Element) {
				Element elementType = (Element)node;
				String name = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
//				KbPlugin.log("NAME: " + name + "; ElemenetTypeName: " + elementTypeName);
				if((ignoreCase && name.equalsIgnoreCase(elementTypeName)) || (!ignoreCase && name.equals(elementTypeName))) {
					return elementType;
				} 
			}
		}
		return null;
	}

	/**
	 * 
	 * @param node
	 * @return
	 */
	public static Properties getAttributes(Element node) {
		Properties attributes = new Properties();

		NamedNodeMap attrs = node.getAttributes();
		for(int j=0; j<attrs.getLength(); j++) {
			Node attr = attrs.item(j);
			attributes.put(attr.getNodeName(), attr.getNodeValue());
		}

		return attributes;
	}

	/**
	 * 
	 * @param elementType
	 * @return
	 */
	public static String getRequaredAttributesAsString(Element elementType) {
		StringBuffer attributes = new StringBuffer();
		ArrayList requaredAttributeTypes = getRequaredAttributeTypes(elementType);
		for(Iterator iter = requaredAttributeTypes.iterator(); iter.hasNext();) {
			Element attributeType = (Element)iter.next();
			String name = attributeType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			attributes.append(' ');
			attributes.append(name);
			attributes.append("=\"");
			attributes.append('"');
		}
		return attributes.toString();
	}

	/**
	 * 
	 * @param elementType
	 * @return
	 */
	public static ArrayList<Element> getRequaredAttributeTypes(Element elementType) {
		ArrayList<Element> requaredAttributeTypes = new ArrayList<Element>();

		ArrayList<Element> attributeTypes = getAttributeTypes(elementType);
		for(Iterator iter = attributeTypes.iterator(); iter.hasNext();) {
			Element attributeType = (Element)iter.next();
			if(checkRequaredAttribute(attributeType)) {
				requaredAttributeTypes.add(attributeType);
			}
		}

		return requaredAttributeTypes;
	}

	/**
	 * 
	 * @param attributeType
	 * @return
	 */
	public static boolean checkRequaredAttribute(Element attributeType) {
		String requared = attributeType.getAttribute(SchemaNodeFactory.REQUIRED_ATTRIBUTE);
		return SchemaNodeFactory.TRUE_REQUIRED_ATTRIBUTE.equals(requared);
	}

	/**
	 * 
	 * @param element
	 * @return
	 */
	public static String getDescription(Element element) {
//		KbPlugin.log("--> KbSchemaUtil.getDescription(Element element)");
//		KbPlugin.log("    element=" + element);
		NodeList nl = element.getElementsByTagName(SchemaNodeFactory.DESCRIPTION_NODE);
		if(nl.getLength()>0) {
			Text text = (Text)nl.item(0).getFirstChild();
			if((text!=null)&&(text.getData()!=null)) {
//				KbPlugin.log("<-- KbSchemaUtil.getDescription(Element element)");
//				KbPlugin.log("    description=" + text.getData().trim());
				return text.getData().trim();
			}
//			KbPlugin.log("<-- KbSchemaUtil.getDescription(Element element)");
//			KbPlugin.log("    description=");
			return "";
		}
//		KbPlugin.log("<-- KbSchemaUtil.getDescription(Element element)");
//		KbPlugin.log("    description=null");
		return null;
	}

	/**
	 * 
	 * @param elementType
	 * @return
	 */
	public static ArrayList<Element> getAttributeTypes(Element elementType) {
		String elementName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
		ArrayList<Element> attributeTypes = new ArrayList<Element>();

		Set<String> attributesD = new HashSet<String>();
		Set<String> attributesI = new HashSet<String>();

		NodeList nodeList = elementType.getElementsByTagName(SchemaNodeFactory.ATTRIBUTE_NODE);
		for(int i = 0; i < nodeList.getLength(); i++) {
			Node child = nodeList.item(i); 
			if(child instanceof Element) {
				Element element = (Element)child;
				attributesD.add(element.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE).toLowerCase());
			}
		}

		NodeList internalTypes = elementType.getElementsByTagName(SchemaNodeFactory.ATTRIBUTE_TYPE_NODE);
		for(int j = 0; j < internalTypes.getLength(); j++) {
			Node child = internalTypes.item(j); 
			if(child instanceof Element) {
				Element element = (Element)child;
				String type = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				String typeLow = type.toLowerCase();
				if(attributesD.contains(typeLow)) {
					attributeTypes.add(element);
					attributesD.remove(typeLow);
					attributesI.add(typeLow);
				} else if(attributesI.contains(typeLow)) {
					KbPlugin.log("Warning: attribute type " + type + " is declared more than once in element " + elementName + ".");
				} else if(!attributesI.contains(typeLow)) {
					KbPlugin.log("Warning: attribute type " + type + " is never used by an attribute in element " + elementName + ".");
				}
			}
		}
		if(attributesD.size() == 0) {
			//Types for all attributes have been found. No need to look for external types. 
			return attributeTypes;
		}
		NodeList externalTypes = elementType.getOwnerDocument().getDocumentElement().getElementsByTagName(SchemaNodeFactory.ATTRIBUTE_TYPE_NODE);
		for(int j = 0; j < externalTypes.getLength(); j++) {
			Node child = externalTypes.item(j); 
			if(child instanceof Element) {
				Element element = (Element)child;
				String type = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				String typeLow = type.toLowerCase();
				if(attributesD.contains(typeLow)) {
					attributeTypes.add(element);
					attributesD.remove(typeLow);
				}
			}
		}
		if(attributesD.size() > 0) {
			Iterator it = attributesD.iterator();
			while(it.hasNext()) {
				String a = it.next().toString();
				KbPlugin.log("Warning: attribute type is not defined for attribute " + a + " in element " + elementName + ".");
			}
		}

		return  attributeTypes;
	}

	/**
	 * 
	 * @param col1
	 * @param col2
	 * @return
	 */
	public static <T> ArrayList<T> addCollection(Collection<T> col1, Collection<T> col2) {
		ArrayList<T> arrayList = new ArrayList<T>();
		Iterator<T> iterator = col1.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		iterator = col2.iterator();
		while(iterator.hasNext()) {
			arrayList.add(iterator.next());
		}
		return arrayList;
	}

	/**
	 * 
	 * @param document
	 * @param tagName
	 * @return
	 */
	public static boolean checkRootElement(Document document, String tagName) {
		String rootElementName = document.getDocumentElement().getAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE);

		return tagName.equals(rootElementName);
	}

	/**
	 * 
	 * @param elementTypes
	 * @param tagName
	 * @return
	 */
	public static TagDescriptor getTagInformationFromElementTypes(Collection elementTypes, String tagName) {
		return getTagInformationFromElementTypes(elementTypes, tagName, null);
	}

	/**
	 * 
	 * @param elementTypes
	 * @param tagName
	 * @param validAttributeValuesTypes
	 * @return
	 */
	public static TagDescriptor getTagInformationFromElementTypes(Collection elementTypes, String tagName, Set validAttributeValuesTypes) {
//		TagDescriptor tagInfo = new TagDescriptor();

		for (Iterator iter = elementTypes.iterator(); iter.hasNext();) {
			Element elementType = (Element)iter.next();
            if(tagName.equals(elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE))) {
            	return getTagInformationFromElementType(elementType, validAttributeValuesTypes);
            }
		}

		return null;
	}

	/**
	 * 
	 * @param elementType
	 * @return
	 */
	public static TagDescriptor getTagInformationFromElementType(Element elementType) {
		return getTagInformationFromElementType(elementType, null);
	}

	/**
	 * 
	 * @param elementType
	 * @param validAttributeValuesTypes
	 * @return
	 */
	public static TagDescriptor getTagInformationFromElementType(Element elementType, Set validAttributeValuesTypes) {
		TagDescriptor tagInfo = new TagDescriptor();

		String bodyContent = elementType.getAttribute(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE);
		String endTag = elementType.getAttribute(SchemaNodeFactory.END_TAG_ATTRIBUTE);

		tagInfo.setBody(SchemaNodeFactory.ALLOWED_BODY_CONTENT_TYPE.equals(bodyContent));
		tagInfo.setClosingTag(!SchemaNodeFactory.REFUSED_TAG_TYPE.equals(endTag));
		tagInfo.setTagName(elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE));

		ArrayList attributeTypes = getAttributeTypes(elementType);
		for(int i=0; i<attributeTypes.size(); i++) {
			Element attributeType = (Element)attributeTypes.get(i);
			AttributeDescriptor attributeInformation = getAttributeInformationFromAttributeType(attributeType, validAttributeValuesTypes);
			tagInfo.addAttributeDescriptor(attributeInformation);
		}

		return tagInfo;
	}

	/**
	 * 
	 * @param attributeType
	 * @return
	 */
	public static AttributeDescriptor getAttributeInformationFromAttributeType(Element attributeType) {
		return getAttributeInformationFromAttributeType(null);
	}

	/**
	 * 
	 * @param attributeType
	 * @param validAttributeValuesTypes
	 * @return
	 */
	public static AttributeDescriptor getAttributeInformationFromAttributeType(Element attributeType, Set validAttributeValuesTypes) {
		AttributeDescriptor descriptor = new AttributeDescriptor();

		String name = attributeType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
		descriptor.setName(name);

		String requiredStr = attributeType.getAttribute(SchemaNodeFactory.REQUIRED_ATTRIBUTE);
		boolean required = SchemaNodeFactory.TRUE_REQUIRED_ATTRIBUTE.equals(requiredStr);
		descriptor.setRequired(required);

		String preferableStr = attributeType.getAttribute(SchemaNodeFactory.PREFERABLE_ATTRIBUTE);
		boolean preferable = SchemaNodeFactory.TRUE_REQUIRED_ATTRIBUTE.equals(preferableStr);
		descriptor.setPreferable(preferable);

		NodeList nodeList = attributeType.getElementsByTagName(SchemaNodeFactory.PROPOSAL_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			Element proposalElement = (Element)nodeList.item(i);

			String type = proposalElement.getAttribute(SchemaNodeFactory.TYPE_ATTRIBUTE);
			if(validAttributeValuesTypes!=null && !validAttributeValuesTypes.contains(type)) {
				continue;
			}
			AttributeValueDescriptor valueDescriptor = new AttributeValueDescriptor();
			valueDescriptor.setType(type);
			descriptor.addValuDescriptor(valueDescriptor);

			NodeList paramList = proposalElement.getElementsByTagName(SchemaNodeFactory.PARAM_NODE);
			for(int j=0; j<paramList.getLength(); j++) {
				Element paramElement = (Element)paramList.item(j);
				String paramName = paramElement.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
				String paramValue = paramElement.getAttribute(SchemaNodeFactory.VALUE_ATTRIBUTE);
				valueDescriptor.addParam(paramName, paramValue);
			}
		}

		return descriptor;
	}
}