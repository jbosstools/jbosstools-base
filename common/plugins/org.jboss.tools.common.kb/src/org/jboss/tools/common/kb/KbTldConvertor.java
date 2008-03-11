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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Properties;
import java.util.StringTokenizer;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.CDATASection;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * Class helps to convert TLD to Schema
 * @author igels
 */
public class KbTldConvertor implements KbSchemaConvertor {

	private final static String TAG_ELEMENT_NAME = "tag";
	private final static String NAME_ELEMENT_NAME = "name";
	private final static String DESCRIPTION_ELEMENT_NAME = "description";
	private final static String ATTRIBUTE_ELEMENT_NAME = "attribute";
	private final static String REQUIRED_ELEMENT_NAME = "required";
	private final static String BODYCONTENT_ELEMENT_NAME_1_1 = "bodycontent";
	private final static String BODYCONTENT_ELEMENT_NAME_1_2 = "body-content";
	private final static String EMPTY_BODYCONTENT_TYPE = "empty";
	private final static String SHORT_NAME_1_1 = "shortname";
	private final static String SHORT_NAME_1_2 = "short-name";

	private static final KbTldConvertor INSTANCE = new KbTldConvertor();

	private KbTldConvertor() {
		super();
	}

	public static KbTldConvertor getInstance() {
		return INSTANCE;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(java.io.InputStream)
	 */
	public Document convertToSchema(InputStream inputStream) {
		Properties attributes = new Properties();
		return convertToSchema(inputStream, attributes, false);
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(org.jboss.tools.common.kb.KbDtdResource)
	 */
	public Document convertToSchema(KbResource resource) {
		KbTldResource tldResource = null;
		if(resource instanceof KbTldResource) {
			tldResource = (KbTldResource)resource;
		} else {
			throw new IllegalArgumentException("KbTldConvertor.convertToSchema(KbResource resource): resource must be instance of KbTldResource");
		}
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.LOCATION_ATTRIBUTE, tldResource.getTldLocation());
		attributes.put(SchemaNodeFactory.JSF_ATTRIBUTE, "" + tldResource.isJsfResource());
		if(tldResource.getUri()!=null) {
			attributes.put(SchemaNodeFactory.URI_ATTRIBUTE, tldResource.getUri());
		}
		attributes.put(SchemaNodeFactory.PREFIX_ATTRIBUTE, tldResource.getFirstPrefix());

		InputStream is = tldResource.getInputStream();
		if(is==null) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.getPluginLog().logWarning("    WARNING! Can't get InputSource from resource (location: " + tldResource.getTldLocation() + ").");
			}
			return null;
		}
		Document doc = convertToSchema(is, attributes, tldResource.isJsfResource());
		String tldContent = tldResource.getTldContent();
		if(doc!=null && tldContent!=null) {
			Element tldContentElement = doc.createElement(SchemaNodeFactory.TLD_CONTENT_NODE);
			CDATASection cdata = doc.createCDATASection(tldContent);
			tldContentElement.appendChild(cdata);
			doc.getDocumentElement().appendChild(tldContentElement);
		}
		return doc;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(java.io.File)
	 */
	public Document convertToSchema(File tldFile) {
		try {
			return convertToSchema(new BufferedInputStream(new FileInputStream(tldFile)), new Properties(), false);
		} catch (FileNotFoundException e) {
			KbPlugin.getPluginLog().logError(e);
			return null;
		}
	}

	/**
	 * 
	 * @param inputStream
	 * @param attributes
	 * @return
	 */
	public Document convertToSchema(InputStream inputStream, Properties attributes, boolean jsfTld) {
		Document tldDocument = null;
		try {
			tldDocument = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(inputStream);
		} catch (Exception e) {
			String message = "ERROR: Can't parse TLD file for converting to the Schema.";
			KbPlugin.getPluginLog().logError(message, e);
			return null;
		}

		Element rootTldElement = tldDocument.getDocumentElement();

		String prefix = getShortName(rootTldElement);
		if((prefix!=null)&&(prefix.length()>0)) {
			attributes.setProperty(SchemaNodeFactory.PREFIX_ATTRIBUTE, prefix);
		}

		Document schema = SchemaNodeFactory.getInstance().createSchemaDocument(attributes);
		addElementTypes(rootTldElement, schema, jsfTld);

		return schema;
	}

	private void addElementTypes(Element rootTldElement, Document schema, boolean jsfTld) {
		NodeList list = rootTldElement.getChildNodes();
		Element rootSchemaElement = schema.getDocumentElement();
		for(int i=0; i<list.getLength(); i++) {
			Node node = list.item(i);
			if(node instanceof Element) {
				if(node.getNodeName().equals(TAG_ELEMENT_NAME)) {
					Element tag = (Element)node;
					addElementType(tag, rootSchemaElement, jsfTld);
				}
			}
		}
	}

	private void addElementType(Element tldTag, Element schemaElement, boolean jsfTld) {
		String tagName = getTagName(tldTag);
		String bodyContentType = getBodyContentType(tldTag);
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.NAME_ATTRIBUTE, tagName);
		attributes.put(SchemaNodeFactory.CONTENT_ATTRIBUTE, SchemaNodeFactory.CONTENT_TYPE_MIXED);
		attributes.put(SchemaNodeFactory.START_TAG_ATTRIBUTE, SchemaNodeFactory.REQUIRED_TAG_TYPE);
		attributes.put(SchemaNodeFactory.END_TAG_ATTRIBUTE, SchemaNodeFactory.REQUIRED_TAG_TYPE);
		attributes.put(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE, bodyContentType);

		Element elementType = SchemaNodeFactory.getInstance().createElementType(schemaElement, attributes);
		String description = getDescription(tldTag);
		if( KbConfigurationFactory.getInstance().getDefaultConfiguration().isUtilizeComments())
			SchemaNodeFactory.getInstance().createDescription(elementType, description);
		addAttributes(tldTag, elementType, jsfTld);
	}

	private void addAttributes(Element tldTag, Element elementType, boolean jsfTld) {
		NodeList list = tldTag.getChildNodes();
		for(int i=0; i<list.getLength(); i++) {
			Node node = list.item(i);
			if(node instanceof Element) {
				if(node.getNodeName().equals(ATTRIBUTE_ELEMENT_NAME)) {
					Element attribute = (Element)node;
					addAttribute(attribute, elementType, jsfTld);
				}
			}
		}
	}

	private void addAttribute(Element tldAttribute, Element elementType, boolean jsfTld) {
		String name = getChildElementBody(tldAttribute, NAME_ELEMENT_NAME);
		String required = getChildElementBody(tldAttribute, REQUIRED_ELEMENT_NAME);
		if(required==null) {
			required = SchemaNodeFactory.FALSE_REQUIRED_ATTRIBUTE;
		}
		String description = getDescription(tldAttribute);
		addAttributeType(tldAttribute, elementType, name, required, description, jsfTld);
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.TYPE_ATTRIBUTE, name);
		SchemaNodeFactory.getInstance().createAttribute(elementType, attributes);
	}

	private void addAttributeType(Element tldAttribute, Element schemaElement, String name, String required, String description, boolean jsfTld) {
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.NAME_ATTRIBUTE, name);
		attributes.put(SchemaNodeFactory.REQUIRED_ATTRIBUTE, required);
		Element attributeType = SchemaNodeFactory.getInstance().createAttributeType(schemaElement, attributes);
		if(description != null &&  KbConfigurationFactory.getInstance().getDefaultConfiguration().isUtilizeComments()) {
			SchemaNodeFactory.getInstance().createDescription(attributeType, description);
		}

		if(jsfTld || tldAttribute.getElementsByTagName("deferred-value").getLength()>0) {
			// Add default proposals
			Element[] proposals = createDefaultELProposals(schemaElement.getOwnerDocument());
			for(int i=0; i<proposals.length; i++) {
				attributeType.appendChild(proposals[i]);
			}
		} else {
			String deferredMethodSignature = getDeferredMethodSignature(tldAttribute);
			if(deferredMethodSignature!=null) {
				// Add method proposal
				Element proposal = createMethodProposal(schemaElement.getOwnerDocument(), deferredMethodSignature);
				if(proposal!=null) {
					attributeType.appendChild(proposal);
				}
			}
		}

	}

	private static Element createMethodProposal(Document document, String signature) {
		String returnType = null;
		String methodName = null;
		Element proposal = null;
		ArrayList<String> paramTypes = new ArrayList<String>();
		StringTokenizer st = new StringTokenizer(signature.trim(), " ()", false);
		if(st.hasMoreTokens()) {
			returnType = st.nextToken(" ");
			if(st.hasMoreTokens()) {
				methodName = st.nextToken("(");
				if(methodName!=null) {
					methodName = methodName.trim();
				}
			}
			if(st.hasMoreTokens()) {
				String params = st.nextToken("()");
				if(params!=null) {
					StringTokenizer stParams = new StringTokenizer(params, ",", false);
					while(stParams.hasMoreTokens()) {
						String param = stParams.nextToken().trim();
						if(param.length()>0) {
							paramTypes.add(param);
						}
					}
				}
			}
		}

		if(returnType!=null && methodName!=null) {
			proposal = document.createElement("proposal");
    		proposal.setAttribute("type", "beanMethodBySignature");
    		Element param = document.createElement("param");
			param.setAttribute("name", "returnType");
			param.setAttribute("value", returnType);
			proposal.appendChild(param);

			for(int i=0; i<paramTypes.size(); i++) {
	    		param = document.createElement("param");
				param.setAttribute("name", "paramType");
				param.setAttribute("value", paramTypes.get(i).toString());
				proposal.appendChild(param);
			}
	   	}

		return proposal;
	}

	private Element[] createDefaultELProposals(Document document) {
		Element jsfBeanProperty = document.createElement("proposal");
		jsfBeanProperty.setAttribute("type", "beanProperty");

		Element jsfBundleProperty = document.createElement("proposal");
		jsfBundleProperty.setAttribute("type", "bundleProperty");

		Element jsfVariables = document.createElement("proposal");
		jsfVariables.setAttribute("type", "jsfVariables");

		return new Element[]{jsfBeanProperty, jsfBundleProperty, jsfVariables};
	}

	private static String getDeferredMethodSignature(Element tldAttribute) {
		NodeList deferredMethods = tldAttribute.getElementsByTagName("deferred-method");
		for(int i=0; i<deferredMethods.getLength(); i++) {
			Element deferredMethod = (Element)deferredMethods.item(i);
			return getChildElementBody(deferredMethod, "method-signature");
		}
		return null;
	}

	private static String getDescription(Element tldElement) {
		return getChildElementBody(tldElement, DESCRIPTION_ELEMENT_NAME);
	}

	private String getTagName(Element tldTag) {
		return getChildElementBody(tldTag, NAME_ELEMENT_NAME);
	}

	private String getBodyContentType(Element tldTag) {
		String type = getChildElementBody(tldTag, BODYCONTENT_ELEMENT_NAME_1_2);
		if(type==null) {
			type = getChildElementBody(tldTag, BODYCONTENT_ELEMENT_NAME_1_1);
		}
		return EMPTY_BODYCONTENT_TYPE.equalsIgnoreCase(type)?SchemaNodeFactory.REFUSED_BODY_CONTENT_TYPE:SchemaNodeFactory.ALLOWED_BODY_CONTENT_TYPE;
	}

	private static String getChildElementBody(Element parentTldElement, String childElemenName) {
		NodeList list = parentTldElement.getChildNodes();
		String body = null;

		for(int i=0; i<list.getLength(); i++) {
			Node node = list.item(i);
			if(node instanceof Element) {
				if(node.getNodeName().equals(childElemenName)) {
					Element element = (Element)node;
					body = getElementBody(element);
					break;
				}
			}
		}
		if(body!=null) {
			body = body.trim();
		}
		return body;
	}

	/**
	 * 
	 * @param element
	 * @return
	 */
	public static String getElementBody(Element element) {
		if (element != null) {
			StringBuffer sb = new StringBuffer();
			NodeList nl = element.getChildNodes();
			for (int i = 0; i < nl.getLength(); i++) {
				Node n = nl.item(i);
				short nodeType = n.getNodeType();
				if (nodeType == Node.TEXT_NODE || nodeType == Node.CDATA_SECTION_NODE) {
					sb.append(((CharacterData)n).getData());
				}
			}
			return sb.toString();
		} else {
			return "";
		}
	}

	/**
	 * 
	 * @param element
	 * @return
	 */
	public String serialize(Element element) {
		StringWriter sw = new StringWriter();
		XMLSerializer ser = new XMLSerializer(sw, createOutputFormat());
		
		try {
			ser.asDOMSerializer();
			ser.serialize(element);
			sw.close();
		} catch (IOException e) {
			KbPlugin.getPluginLog().logError(e);
		}	
		return sw.toString();

	}

	private String getShortName(Element rootTldElement) {
		String shortName = getChildElementBody(rootTldElement, SHORT_NAME_1_2);
		if((shortName==null)||(shortName.length()<1)) {
			shortName = getChildElementBody(rootTldElement, SHORT_NAME_1_1);
		}
		return shortName;
	}

	/**
	 * 
	 * @return
	 */
	public OutputFormat createOutputFormat() {
		OutputFormat format = new OutputFormat("xml", "UTF-8", true);
		format.setLineSeparator("\r\n");
		format.setIndent(4);
		return format;
	}
}