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

import java.util.Enumeration;
import java.util.Properties;

import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * @author eskimo
 */
public class SchemaNodeFactory {
	
	public static final String SCHEMA_NODE = "Schema";
	public static final String ELEMENT_TYPE_NODE = "ElementType";
	public static final String ATTRIBUTE_TYPE_NODE = "AttributeType";
	public static final String TLD_CONTENT_NODE = "TldContent";
	public static final String ELEMENT_NODE = "element";
	public static final String ATTRIBUTE_NODE = "attribute";
	public static final String PARAM_NODE = "param";
	public static final String GROUP_NODE = "group";
	public static final String DESCRIPTION_NODE = "description";
	public static final String PREFIX_ATTRIBUTE = "prefix";
	public static final String NAME_ATTRIBUTE = "name";
	public static final String IGNORE_CASE_ATTRIBUTE = "ignoreCase";
	public static final String TYPE_ATTRIBUTE = "type";
	public static final String PROPOSAL_NODE = "proposal";
	public static final String CONTENT_ATTRIBUTE = "content";
	public static final String DEFAULT_ATTRIBUTE = "default";
	public static final String REQUIRED_ATTRIBUTE = "required";
	public static final String PREFERABLE_ATTRIBUTE = "preferable";
	public static final String URI_ATTRIBUTE = "uri";
	public static final String URL_ATTRIBUTE = "url";
	public static final String VERSION_ATTRIBUTE = "version";
	public static final String JSF_ATTRIBUTE = "jsf";
	public static final String LOCATION_ATTRIBUTE = "location";
	public static final String ROOT_ELEMENT_ATTRIBUTE = "rootElement";
	public static final String VALUES_ATTRIBUTE = "values";
	public static final String VALUE_ATTRIBUTE = "value";
	public static final String ENUMERATION_TYPE = "enumeration";
	public static final String BUNDLE_NAME_TYPE = KbDinamicResource.BUNDLE_NAME_TYPE;
	public static final String ENUMERATION_SEPARATOR = ",";
	public static final String ATTRIBUTE_TYPE_SEPARATOR = ",";
	public static final String CONTENT_TYPE_MIXED = "mixed";
	public static final String TRUE_REQUIRED_ATTRIBUTE = "true";
	public static final String FALSE_REQUIRED_ATTRIBUTE = "false";
	public static final String START_TAG_ATTRIBUTE = "startTag";
	public static final String END_TAG_ATTRIBUTE = "endTag";
	public static final String OPTIONAL_TAG_TYPE = "optional";
	public static final String REQUIRED_TAG_TYPE = "required";
	public static final String REFUSED_TAG_TYPE = "refused";
	public static final String BODY_CONTENT_ATTRIBUTE = "bodyContent";
	public static final String ALLOWED_BODY_CONTENT_TYPE = "allowed";
	public static final String REFUSED_BODY_CONTENT_TYPE = "refused";
	public static final String OPTIONAL_BODY_CONTENT_TYPE = "optional";
	
	public static final char WILD_CARD_CHAR = '*';

	/**
	 *  
	 * @return
	 */
	public Document createSchemaDocument(Properties attributes) {
		Document document = KbDocumentBuilderFactory.createDocumentBuilder(false).newDocument();
		Element element = createDocumentElement(SCHEMA_NODE, document, new Properties());
		initAttributes(element, attributes);
		document.appendChild(element);
		return document; 
	}

	/**
	 * 
	 * @param elementName
	 * @param document
	 * @param attributes
	 * @return
	 */
	public static Element createDocumentElement(String elementName, Document document, Properties attributes) {
		Element newElement = document.createElement(elementName);
		initAttributes(newElement, attributes);
		return newElement;
	}

	/**
	 * 
	 * @param parent
	 * @param attributes
	 * @return
	 */
	public Element createElementType(Element parent, Properties attributes) {
		Element element = createDocumentElement(ELEMENT_TYPE_NODE, parent.getOwnerDocument(), attributes);
		parent.appendChild(element);
		return element;
	}

	/**
	 * 
	 * @param parent
	 * @param attributes
	 * @return
	 */
	public Element createElement(Element parent, Properties attributes) {
		Element element = createDocumentElement(ELEMENT_NODE, parent.getOwnerDocument(), attributes);
		parent.appendChild(element);
		return element;
	}

	/**
	 * 
	 * @param parent
	 * @param attributes
	 * @return
	 */
	public Element createAttributeType(Element parent, Properties attributes) {
		Element element = createDocumentElement(ATTRIBUTE_TYPE_NODE, parent.getOwnerDocument(), attributes);
		parent.appendChild(element);
		return element;
	}

	/**
	 * 
	 * @param parent
	 * @param attributes
	 * @return
	 */
	public Element createAttribute(Element parent, Properties attributes) {
		Element element = createDocumentElement(ATTRIBUTE_NODE, parent.getOwnerDocument(), attributes);
		parent.appendChild(element);
		return element;
	}

	/**
	 * 
	 * @param parent
	 * @param attributes
	 * @return
	 */
	public Element createGroup(Element parent, Properties attributes) {
		Element element = createDocumentElement(GROUP_NODE, parent.getOwnerDocument(), attributes);
		parent.appendChild(element);
		return element;
	}

	/**
	 * 
	 * @param parent
	 * @param body
	 * @return
	 */
	public Element createDescription(Element parent, String body) {
		Element element = createDocumentElement(DESCRIPTION_NODE, parent.getOwnerDocument(), new Properties());
		element.appendChild(element.getOwnerDocument().createTextNode(body));
		parent.appendChild(element);
		return element;
	}

	private static void initAttributes(Element element, Properties attributes) {
		Enumeration en = attributes.keys();
		while(en.hasMoreElements()) {
			String key = (String)en.nextElement();
			element.setAttribute(key, attributes.getProperty(key));
		}
	}

	/**
	 * 
	 * @return
	 */
	public static SchemaNodeFactory getInstance() {
		return SchemaNodeFactoryHolder.INSTANCE;
	}

	public static class SchemaNodeFactoryHolder {
		public static final SchemaNodeFactory INSTANCE = new SchemaNodeFactory();
	}
}