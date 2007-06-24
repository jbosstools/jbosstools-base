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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Vector;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;
import com.wutka.dtd.DTD;
import com.wutka.dtd.DTDAttribute;
import com.wutka.dtd.DTDComment;
import com.wutka.dtd.DTDContainer;
import com.wutka.dtd.DTDDecl;
import com.wutka.dtd.DTDElement;
import com.wutka.dtd.DTDEnumeration;
import com.wutka.dtd.DTDItem;
import com.wutka.dtd.DTDName;
import com.wutka.dtd.DTDParser;

/**
 * @author igels
 */
public class KbDtdConvertor implements KbSchemaConvertor {


	private static final KbDtdConvertor INSTANCE = new KbDtdConvertor();

	private KbDtdConvertor() {
		super();
	}

	/**
	 * 
	 * @return
	 */
	public static KbDtdConvertor getInstance() {
		return INSTANCE;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(java.io.InputStream)
	 */
	public Document convertToSchema(InputStream inputStream) {
		return convertToSchema(new InputStreamReader(inputStream), new Properties());
	}

	/**
	 * 
	 * @param reader
	 * @param attributes
	 * @return
	 */
	public Document convertToSchema(Reader reader, Properties attributes) {
		DTD dtdRoot;
		Document document=null;
		try {
			DTDParser parser = new DTDParser(reader);
			dtdRoot = parser.parse();

			document = prepareDocument(dtdRoot, attributes);
//			Object[] objs = dtdRoot.getItems();

			// first add Attributes and Elements definitions			
			List elements = Arrays.asList(dtdRoot.getItems());		

			if(elements.size() > 0){
//				set root element
				if(elements.size()!=0) {
					int i=0;
					// find first dtd element 
					while(! (elements.get(i) instanceof DTDElement))i++; 			
					DTDElement element = (DTDElement)elements.get(i);	
					document.getDocumentElement().setAttribute(SchemaNodeFactory.ROOT_ELEMENT_ATTRIBUTE, element.getName());					
				}
			}

			for(int i=0;i<elements.size();i++) {
				if (elements.get(i) instanceof DTDElement) {
					DTDComment previousComment = null;				
					if(i!=0 && (elements.get(i-1) instanceof DTDComment)) previousComment = (DTDComment)elements.get(i-1);
					prepareElementTypeNode(document.getDocumentElement(),(DTDElement)elements.get(i),previousComment);
				}

			}

		} catch (IOException e) {
			KbPlugin.log(e);
		}

		return document;
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(org.jboss.tools.common.kb.KbDtdResource)
	 */
	public Document convertToSchema(KbResource resource) {
		if(KbPlugin.isDebugEnabled()) {
			KbPlugin.log("--> KbDtdConvertor.convertToSchema(KbResource resource)");
			KbPlugin.log("    resource = " + resource);
		}

		//		KbPlugin.log("convert resource - " + resource);
		KbDtdResource dtdResource = null;
		if(resource instanceof KbDtdResource) {
			dtdResource = (KbDtdResource)resource;
		} else {
			throw new IllegalArgumentException("KbDtdConvertor.convertToSchema(KbResource resource): resource must be instance of KbDtdResource");
		}
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.URI_ATTRIBUTE, dtdResource.getUri());
		attributes.put(SchemaNodeFactory.URL_ATTRIBUTE, dtdResource.getId());

		InputStream is = dtdResource.getInputStream();
		if(is==null) {
			if(KbPlugin.isDebugEnabled()) {
				KbPlugin.log("    Can't get Input Stream from resource.");
				KbPlugin.log("<-- KbDtdConvertor.convertToSchema(KbResource resource)");
				KbPlugin.log("    return = null");
			}
			return null;
		}
		return convertToSchema(new InputStreamReader(is), attributes);
	}

	/**
	 * @see org.jboss.tools.common.kb.KbSchemaConvertor#convertToSchema(java.io.File)
	 */
	public Document convertToSchema(File dtdFile) {
		try {
			return convertToSchema(new FileReader(dtdFile), new Properties());
		} catch (FileNotFoundException e) {
			KbPlugin.log(e);
			return null;
		}
	}

	private Document prepareDocument(DTD dtd, Properties attributes) {
//		Vector notations = dtd.getItemsByType(DTDNotation.class);
//		KbPlugin.log("Notation URI = " + notations.size());
		Document document =  SchemaNodeFactory.getInstance().createSchemaDocument(attributes);
		return document;
	}
	
	public static final String EMPTY = "EMPTY";
	public static final String ANY = "ANY";
	public static final String PCDATA = "PCDATA";		
	
	private void prepareElementTypeNode(Element parent,DTDElement dtdElement, DTDComment comment) {
		Properties attributes = new Properties();
		prepareElementTypeAttributes(dtdElement,attributes);
	
		DTDItem item = dtdElement.getContent();
		Collection<String> content = new ArrayList<String>();
		getElementContent(item,content);	

		if(content.size()==1) {
			Object element = content.iterator().next();
			if(EMPTY.equalsIgnoreCase(element.toString())) 
				attributes.put(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE,SchemaNodeFactory.REFUSED_BODY_CONTENT_TYPE);		
		} else {
			attributes.put(SchemaNodeFactory.BODY_CONTENT_ATTRIBUTE,SchemaNodeFactory.ALLOWED_BODY_CONTENT_TYPE);			
		}
	
		Element child = SchemaNodeFactory.getInstance().createElementType(parent,attributes);
		if(comment!=null && KbConfigurationFactory.getInstance().getDefaultConfiguration().isUtilizeComments()) {
			SchemaNodeFactory.getInstance().createDescription(child,comment.getText());
		}

		Collection values = dtdElement.attributes.values();

		for (Iterator iter = values.iterator(); iter.hasNext();) {
			DTDAttribute element = (DTDAttribute) iter.next();
			prepareAttributeTypeNode(child,element);
			prepareAttributeNode(child,element);
		}
		
		for(Iterator i=content.iterator();i.hasNext();) {
			String contentItem = i.next().toString();
			if(ANY.equalsIgnoreCase(contentItem) || EMPTY.equalsIgnoreCase(contentItem) || PCDATA.equalsIgnoreCase(contentItem)) continue;
			prepareElementNode(child,contentItem.toString());
		}
	}
	
	private void getElementContent(DTDItem item, Collection<String> content) {
		if(item instanceof DTDContainer) {
			DTDContainer container = (DTDContainer)item;
			for(int i=0;i<container.getItems().length;i++) {
				getElementContent(container.getItem(i),content);	
			}
		}if(item instanceof DTDName) {
			String name = ((DTDName)item).getValue();
			if(!content.contains(name)) content.add(name);
			return;
		}
	}
	
	private void prepareAttributeTypeNode(Element element,DTDAttribute dtdElement) {
		Properties attributes = new Properties();
		prepareAttributeTypeAttributes(dtdElement,attributes);
		SchemaNodeFactory.getInstance().createAttributeType(element,attributes);	
					
	}

	private void prepareAttributeNode(Element element,DTDAttribute dtdAttribute) {
		Properties attributes = new Properties();
		prepareAttributeAttributes(dtdAttribute,attributes);
		SchemaNodeFactory.getInstance().createAttribute(element,attributes);								
	}

	private void prepareElementNode(Element element,String name) {
		Properties attributes = new Properties();
		attributes.put(SchemaNodeFactory.TYPE_ATTRIBUTE,name);		
		SchemaNodeFactory.getInstance().createElement(element,attributes);
	}
	
	private void prepareElementTypeAttributes(DTDElement element,Properties attributes) {
//		DTDItem item = element.getContent();
		attributes.put(SchemaNodeFactory.NAME_ATTRIBUTE,element.getName());
		attributes.put(SchemaNodeFactory.START_TAG_ATTRIBUTE,SchemaNodeFactory.REQUIRED_TAG_TYPE);				
		attributes.put(SchemaNodeFactory.END_TAG_ATTRIBUTE,SchemaNodeFactory.REQUIRED_TAG_TYPE);		
	}

	private void prepareAttributeTypeAttributes(DTDAttribute element,Properties attributes) {
		attributes.put(SchemaNodeFactory.NAME_ATTRIBUTE,element.getName());
		attributes.put(SchemaNodeFactory.DEFAULT_ATTRIBUTE,element.getDefaultValue()==null?"":element.getDefaultValue());
		String enumList = getEnumValues(element);
		if(!"".equals(enumList)) { 
			attributes.put(SchemaNodeFactory.TYPE_ATTRIBUTE,SchemaNodeFactory.ENUMERATION_TYPE);
			attributes.put(SchemaNodeFactory.VALUES_ATTRIBUTE,getEnumValues(element));			
		}
		attributes.put(SchemaNodeFactory.REQUIRED_ATTRIBUTE,(element.getDecl() == DTDDecl.REQUIRED)+"");				
	}

	private void prepareAttributeAttributes(DTDAttribute attribute,Properties attributes) {
		attributes.put(SchemaNodeFactory.DEFAULT_ATTRIBUTE,attribute.getDefaultValue()==null?"":attribute.getDefaultValue());
		attributes.put(SchemaNodeFactory.TYPE_ATTRIBUTE,attribute.getName());
		attributes.put(SchemaNodeFactory.REQUIRED_ATTRIBUTE,(attribute.getDecl() == DTDDecl.REQUIRED)+"");		
			
	}
	
	public void prepareCommentNode(Element element, DTDComment comment) {
		SchemaNodeFactory.getInstance().createDescription(element,comment.getText());
	}

	private String getEnumValues(DTDAttribute attribute) {
		if(attribute.getType() instanceof DTDEnumeration) {
			DTDEnumeration enumeration = (DTDEnumeration)attribute.getType();
			Vector items = enumeration.getItemsVec();
			String enumList = "";
			for(int i=0;i<items.size();i++) {
				enumList+=items.get(i).toString()+",";	
			}
			return enumList.substring(0,enumList.length()-1);
		}
		return "";
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
			KbPlugin.log(e);
		}	
		return sw.toString();

	}

	/**
	 * 
	 * @return
	 */
	public OutputFormat createOutputFormat() {
		OutputFormat format = new OutputFormat("xml", "UTF-8", true);
		format.setLineSeparator("\n");
		format.setIndent(2);
		return format;
	}
}