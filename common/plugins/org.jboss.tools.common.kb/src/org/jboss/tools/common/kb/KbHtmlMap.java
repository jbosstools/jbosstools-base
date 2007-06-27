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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author igels
 */
public class KbHtmlMap {
	private final static String ROOT_ELEMENT_ATTRIBUTE_VALUE = "HTML";

	private HashMap htmlMap;
	private static File hashMapLocation = new File(KbConfigurationFactory.getInstance().getDefaultConfiguration().getHtmlMapFilePath());

	/**
	 * 
	 */
	private KbHtmlMap(HashMap proposals) {
		htmlMap = proposals;
	}

	public static KbHtmlMap parseSchema(Document schema) {
		HashMap<String,KbProposal[]> map = new HashMap<String,KbProposal[]>();
		NodeList nodeList = schema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			HashMap<String,Element> children = new HashMap<String,Element>();
			Element elementType = (Element)nodeList.item(i);
			String tagName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			ArrayList childrenElementTypes = getChildElementTypes(schema, tagName);
			addOptionalTags(children, elementType);
			for(int j=0; j<childrenElementTypes.size(); j++) {
				Element childrenElementType = (Element)childrenElementTypes.get(j);
				addOptionalTags(children, childrenElementType);
			}
			ArrayList<KbProposal> proposalList = getTagProposal(children);
			KbProposal[] proposals = (KbProposal[])proposalList.toArray(new KbProposal[proposalList.size()]);
			map.put(tagName, proposals);
		}
		return new KbHtmlMap(map);
	}

	public static Document convertSchema(Document schema) {
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE, ROOT_ELEMENT_ATTRIBUTE_VALUE);
		Document document = SchemaNodeFactory.getInstance().createSchemaDocument(attributes);
		Element schemaElement = document.getDocumentElement();

		NodeList nodeList = schema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int i=0; i<nodeList.getLength(); i++) {
			HashMap<String,Element> children = new HashMap<String,Element>();
			Element elementType = (Element)nodeList.item(i);
			String tagName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			ArrayList childrenElementTypes = getChildElementTypes(schema, tagName);
			addOptionalTags(children, elementType);
			for(int j=0; j<childrenElementTypes.size(); j++) {
				Element childrenElementType = (Element)childrenElementTypes.get(j);
				addOptionalTags(children, childrenElementType);
			}

			attributes = KbSchemaUtil.getAttributes(elementType);
			Element newElementType = SchemaNodeFactory.getInstance().createElementType(schemaElement, attributes);

			NodeList attributeTypes = elementType.getElementsByTagName(SchemaNodeFactory.ATTRIBUTE_TYPE_NODE);
			for(int j=0; j<attributeTypes.getLength(); j++) {
				Element attribute = (Element)attributeTypes.item(j);
				attributes = KbSchemaUtil.getAttributes(attribute);
				SchemaNodeFactory.getInstance().createAttributeType(newElementType, attributes);
			}

			NodeList attributeElements = elementType.getElementsByTagName(SchemaNodeFactory.ATTRIBUTE_NODE);
			for(int j=0; j<attributeElements.getLength(); j++) {
				Element attribute = (Element)attributeElements.item(j);
				attributes = KbSchemaUtil.getAttributes(attribute);
				SchemaNodeFactory.getInstance().createAttribute(newElementType, attributes);
			}

			for(Iterator iter = children.values().iterator();iter.hasNext();) {
				Element element = (Element)iter.next();
				attributes = new Properties();
				attributes.put(SchemaNodeFactory.TYPE_ATTRIBUTE, element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE));
				SchemaNodeFactory.getInstance().createElement(newElementType, attributes);
			}
		}

		return document;
	}

	public static boolean convertSchemaToFile(Document schema, File schemaFile) {
		Document convertedSchema = convertSchema(schema);
		try {
			Element element = convertedSchema.getDocumentElement();
			StringWriter sw = new StringWriter();
			XMLSerializer ser = new XMLSerializer(sw, createOutputFormat());

			ser.asDOMSerializer();
			ser.serialize(element);
			sw.close();

			BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(schemaFile));
			os.write(sw.toString().getBytes());
			os.flush();
			os.close();

//			TransformerFactory.newInstance().newTransformer().transform(new DOMSource(document), new StreamResult(schemaFile));
		} catch (Exception e) {
			String message = "ERROR: Can't serialize HTML schema to file :" + schemaFile;
			KbPlugin.getPluginLog().logError(message, e);
			schemaFile.deleteOnExit();
			return false;
		}
		return true;
	}

	private static OutputFormat createOutputFormat() {
		OutputFormat format = new OutputFormat("xml", "UTF-8", true);
		format.setLineSeparator("\n");
		format.setIndent(2);
		return format;
	}

	private static void addOptionalTags(HashMap<String,Element> map, Element elementType) {
//		KbPlugin.log("--> KbHtmlMap.addRefuseTag(HashMap map, Element elementType, String tagName, boolean mask)");
		String elementTypeName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
//		KbPlugin.log("    map size=" + map.size());
//		KbPlugin.log("    elementType=" + elementTypeName);
		if(!map.containsKey(elementTypeName)) {
			map.put(elementTypeName, elementType);
			String startTagType = elementType.getAttribute(SchemaNodeFactory.START_TAG_ATTRIBUTE);
			if(SchemaNodeFactory.OPTIONAL_TAG_TYPE.equals(startTagType)) {
//				KbPlugin.log("    Tag have optional start tag");
				ArrayList childElementTypes = getChildElementTypes(elementType.getOwnerDocument(), elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE));
				for(Iterator iter = childElementTypes.iterator(); iter.hasNext();) {
					Element childElementType = (Element)iter.next();
					addOptionalTags(map, childElementType);
				}
			}
		}
//		KbPlugin.log("<-- KbHtmlMap.addRefuseTag(HashMap map, Element elementType, String tagName, boolean mask)");
//		KbPlugin.log("    map size=" + map.size());
	}

	private static ArrayList<Element> getChildElementTypes(Document schema, String tagName) {
		ArrayList<Element> childElementTypes = new ArrayList<Element>();

		ArrayList childElements = getChildElements(schema, tagName);

		for(Iterator iter = childElements.iterator(); iter.hasNext();) {
			Element element = (Element)iter.next();
			Element elementType = KbSchemaUtil.getElementTypeByElement(element);
			if(elementType!=null) {
				childElementTypes.add(elementType);
			}
		}

		return childElementTypes; 
	}

	private static ArrayList<Element> getChildElements(Document schema, String elementTypeName) {
//		KbPlugin.log("--> getChildElements(Collection resources, String elementTypeName, boolean rootTag)");
//		KbPlugin.log("    resources size = " + resources.size());
//		KbPlugin.log("    elementTypeName  = " + elementTypeName);

		ArrayList<Element> elements = new ArrayList<Element>();

//		ArrayList documents = new ArrayList();

		NodeList elementTypes = schema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int j=0; j<elementTypes.getLength(); j++) {
			Node child = elementTypes.item(j); 
			if(child instanceof Element) {
				Element elementType = (Element)child;
				if(elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE).equalsIgnoreCase(elementTypeName)) {
					NodeList childElements = elementType.getElementsByTagName(SchemaNodeFactory.ELEMENT_NODE);
					for(int g=0; g<childElements.getLength(); g++) {
						Node childElement = childElements.item(g);
						if(childElement instanceof Element) {
							elements.add((Element)childElement);
						}
					} 
				}
			}
		}

//		KbPlugin.log("<-- getChildElements(Collection resources, String elementTypeName, boolean rootTag)");
//		KbPlugin.log("    element size = " + elements.size());
		return elements;
	}

	private static ArrayList<KbProposal> getTagProposal(HashMap elements) {
		return getProposal(elements.values(), true);
	}

	private static ArrayList<KbProposal> getProposal(Collection elements, boolean tag) {
//		KbPlugin.log("--> getProposal(ArrayList elements, boolean tag)");
//		KbPlugin.log("    elemnets size = " + elements.size());
//		KbPlugin.log("    tag = " + tag);

		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(Iterator iter = elements.iterator();iter.hasNext();) {
			Element element = (Element)iter.next();
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
				}
				proposal.setPosition(position);
				proposal.setIcon(KbIcon.HTML_TAG);
			} else {
				proposal.setReplacementString(label);
				proposal.setIcon(KbIcon.HTML_ATTRIBUTE);
			}

//			KbPlugin.log("    add kbProposals:\n" + proposal.toString());
//			KbPlugin.log("    for element: " + element);

			kbProposals.add(proposal);
		}

//		KbPlugin.log("<-- getProposal()");
//		KbPlugin.log("    kbProposals size = " + kbProposals.size());

		return kbProposals;
	}

	public KbProposal[] getChildren(String tagName) {
		return (KbProposal[])htmlMap.get(tagName);
	}

	public boolean serialize() {
		boolean res = true;
		try {
			FileOutputStream fos = new FileOutputStream(hashMapLocation);
			ObjectOutputStream oos = new ObjectOutputStream(fos);
			oos.writeObject(htmlMap);
			oos.flush();
			oos.close();
		} catch (Exception e) {
			res = false;
			String message = "ERROR: Can't serialize html map to file: " + hashMapLocation;
			KbPlugin.getPluginLog().logError(message, e);
		}

		return res;
	}

	public static KbHtmlMap loadMap() throws IOException, ClassNotFoundException {
		HashMap map = null;
		FileInputStream fis = new FileInputStream(hashMapLocation);
		ObjectInputStream ois = new ObjectInputStream(fis);
		map = (HashMap)ois.readObject();
		ois.close();
		return new KbHtmlMap(map);
	}
}