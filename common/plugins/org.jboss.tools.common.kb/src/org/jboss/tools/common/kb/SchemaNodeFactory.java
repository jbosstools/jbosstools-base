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

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * @author eskimo
 */
public class SchemaNodeFactory {
	
	public static final String SCHEMA_NODE = "schema"; //$NON-NLS-1$
	public static final String ELEMENT_TYPE_NODE = "element-type"; //$NON-NLS-1$
	public static final String ATTRIBUTE_TYPE_NODE = "attribute-type"; //$NON-NLS-1$
	public static final String TLD_CONTENT_NODE = "tld-content"; //$NON-NLS-1$
	public static final String ELEMENT_NODE = "element"; //$NON-NLS-1$
	public static final String ATTRIBUTE_NODE = "attribute"; //$NON-NLS-1$
	public static final String PARAM_NODE = "param"; //$NON-NLS-1$
	public static final String GROUP_NODE = "group"; //$NON-NLS-1$
	public static final String DESCRIPTION_NODE = "description"; //$NON-NLS-1$
	public static final String PREFIX_ATTRIBUTE = "prefix"; //$NON-NLS-1$
	public static final String NAME_ATTRIBUTE = "name"; //$NON-NLS-1$
	public static final String IGNORE_CASE_ATTRIBUTE = "ignore-case"; //$NON-NLS-1$
	public static final String TYPE_ATTRIBUTE = "type"; //$NON-NLS-1$
	public static final String PROPOSAL_NODE = "proposal"; //$NON-NLS-1$
	public static final String CONTENT_ATTRIBUTE = "content"; //$NON-NLS-1$
	public static final String DEFAULT_ATTRIBUTE = "default"; //$NON-NLS-1$
	public static final String REQUIRED_ATTRIBUTE = "required"; //$NON-NLS-1$
	public static final String PREFERABLE_ATTRIBUTE = "preferable"; //$NON-NLS-1$
	public static final String URI_ATTRIBUTE = "uri"; //$NON-NLS-1$
	public static final String URL_ATTRIBUTE = "url"; //$NON-NLS-1$
	public static final String VERSION_ATTRIBUTE = "version"; //$NON-NLS-1$
	public static final String JSF_ATTRIBUTE = "jsf"; //$NON-NLS-1$
	public static final String LOCATION_ATTRIBUTE = "location"; //$NON-NLS-1$
	public static final String ROOT_ELEMENT_ATTRIBUTE = "root-element"; //$NON-NLS-1$
	public static final String VALUES_ATTRIBUTE = "values"; //$NON-NLS-1$
	public static final String VALUE_ATTRIBUTE = "value"; //$NON-NLS-1$
	public static final String ENUMERATION_TYPE = "enumeration"; //$NON-NLS-1$
	public static final String BUNDLE_NAME_TYPE = KbDinamicResource.BUNDLE_NAME_TYPE;
	public static final String ENUMERATION_SEPARATOR = ","; //$NON-NLS-1$
	public static final String ATTRIBUTE_TYPE_SEPARATOR = ","; //$NON-NLS-1$
	public static final String CONTENT_TYPE_MIXED = "mixed"; //$NON-NLS-1$
	public static final String TRUE_REQUIRED_ATTRIBUTE = "true"; //$NON-NLS-1$
	public static final String FALSE_REQUIRED_ATTRIBUTE = "false"; //$NON-NLS-1$
	public static final String START_TAG_ATTRIBUTE = "start-tag"; //$NON-NLS-1$
	public static final String END_TAG_ATTRIBUTE = "end-tag"; //$NON-NLS-1$
	public static final String OPTIONAL_TAG_TYPE = "optional"; //$NON-NLS-1$
	public static final String REQUIRED_TAG_TYPE = "required"; //$NON-NLS-1$
	public static final String REFUSED_TAG_TYPE = "refused"; //$NON-NLS-1$
	public static final String BODY_CONTENT_ATTRIBUTE = "body-content"; //$NON-NLS-1$
	public static final String ALLOWED_BODY_CONTENT_TYPE = "allowed"; //$NON-NLS-1$
	public static final String REFUSED_BODY_CONTENT_TYPE = "refused"; //$NON-NLS-1$
	public static final String OPTIONAL_BODY_CONTENT_TYPE = "optional"; //$NON-NLS-1$
	
	public static final char WILD_CARD_CHAR = '*';

	/**
	 *  
	 * @return
	 */
	public Document createSchemaDocument(Properties attributes) {
		Document document = createSchemaDocument(KbDocumentBuilderFactory.createDocumentBuilder(false),attributes);
		return document; 
	}
	
	/**
	 *  
	 * @return
	 */
	public Document createSchemaDocument(DocumentBuilder builder, Properties attributes) {
		Document document = builder.newDocument();
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