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
import java.util.StringTokenizer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.kb.configuration.KbConfigurationFactory;

/**
 * @author igels
 */
public class KbJspDirectiveStore implements KbStore {

	private final static TagDescriptor DEFAULT_TAG_INFO;
	static {
		DEFAULT_TAG_INFO = new TagDescriptor();
		DEFAULT_TAG_INFO.setBody(false);
		DEFAULT_TAG_INFO.setClosingTag(false);
	}

	private String jspDirectiveSchemaLocation;
	private Document jspDirectiveSchema;

	private static final KbJspDirectiveStore INSTANCE = new KbJspDirectiveStore();

	private KbJspDirectiveStore() {
		jspDirectiveSchemaLocation = KbConfigurationFactory.getInstance().getDefaultConfiguration().getJspDirectiveSchemaFilePath();
	}

	public static KbJspDirectiveStore getInstance() {
		return INSTANCE;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.kb.KbStore#queryTagInformation(org.jboss.tools.common.kb.KbQuery)
	 */
	public TagDescriptor queryTagInformation(KbQuery query) {
		return DEFAULT_TAG_INFO;
	}

	public AttributeDescriptor queryAttributeInformation(KbQuery query) {
		// TODO
		throw new RuntimeException("This method is not implemented yet.");
	}

	public Collection<KbProposal> queryProposal(KbQuery query) {
		if(!activateJspDirectiveSchema()) {
			return new ArrayList<KbProposal>();
		} 

		ArrayList<KbProposal> proposals = queryTagProposal(query);
		Collections.sort(proposals);

		return proposals;
	}

	private ArrayList<KbProposal> queryTagProposal(KbQuery query) {
//		KbPlugin.log("--> KbJspDirectiveStore.queryProposal(KbQuery query)");
		String strQuery = query.getQuery();
//		KbPlugin.log("    query=\"" + query + "\"");

		if((!strQuery.startsWith(KbQuery.JSP_DIRECTIVE_QUERY))||
		   (strQuery.startsWith(KbQuery.JSP_DIRECTIVE_QUERY + KbQuery.ATTRIBUTE_SEPARATOR))||
		   (strQuery.indexOf(KbQuery.ATTRIBUTE_SEPARATOR + KbQuery.ENUMERATION_SEPARATOR)!=-1)) {

			String errorMessage = "ERROR: Bad query: " + strQuery + "\n" + 
								  "       Possible query format: \"" + KbQuery.JSP_DIRECTIVE_QUERY + "directiveName" + KbQuery.ATTRIBUTE_SEPARATOR + "attributeName" + KbQuery.ENUMERATION_SEPARATOR + "attributeValue\"";
//			KbPlugin.log(errorMessage);
			throw new RuntimeException(errorMessage);
//			return new ArrayList();
		} else if(strQuery.length() == KbQuery.JSP_DIRECTIVE_QUERY.length()) {
			return getDirectives("");
		}

		int startAttributeName = strQuery.lastIndexOf(KbQuery.ATTRIBUTE_SEPARATOR);
		if((startAttributeName == 0)||(startAttributeName < 0)){
			return getDirectives(strQuery.substring(KbQuery.JSP_DIRECTIVE_QUERY.length()));
		}

		String directiveName = strQuery.substring(KbQuery.JSP_DIRECTIVE_QUERY.length(), startAttributeName);
		startAttributeName+=KbQuery.ATTRIBUTE_SEPARATOR.length();
		if(startAttributeName == strQuery.length()) {
			return getAttributes(directiveName, "");
		}

		int startAttributeValue = strQuery.indexOf(KbQuery.ENUMERATION_SEPARATOR);
		if(startAttributeValue < 0) {
			return getAttributes(directiveName, strQuery.substring(startAttributeName));
		}

		String attributeName = strQuery.substring(startAttributeName, startAttributeValue);
		startAttributeValue+=KbQuery.ENUMERATION_SEPARATOR.length();
		if(startAttributeValue == strQuery.length()) {
			return getEnumeration(directiveName, attributeName, "");
		}

//		KbPlugin.log("<-- queryProposal()");
		return getEnumeration(directiveName, attributeName, KbQuery.decode(strQuery.substring(startAttributeValue)));
	}

	public void registerResource(KbResource resource) {
	}

	public void unregisterResource(KbResource resource) {
	}

	private synchronized boolean activateJspDirectiveSchema() {
		if(jspDirectiveSchema != null) {
			return true;
		}

		File schemaLocation = new File(jspDirectiveSchemaLocation);
		if((schemaLocation == null)||(!schemaLocation.exists())) {
//			String errorMessage = "ERROR: Html schema (location: " + schemaLocation + ") does not exist!"; 
//			KbPlugin.log(errorMessage);
//			throw new RuntimeException(errorMessage);
			return false;
		}

		try {
			jspDirectiveSchema = KbDocumentBuilderFactory.createDocumentBuilder(false).parse(schemaLocation);
		} catch (Exception e) {
			String message = "ERROR: Can't parse JSP Directive Schema (location: " + schemaLocation + ")";
			KbPlugin.getPluginLog().logError(message, e);
			return false;
		}
		return true;
	}

	private ArrayList<KbProposal> getDirectives(String directiveMask) {
		return getDirectiveProposal(getElementTypesByName(directiveMask, true));
	}

	private ArrayList<KbProposal> getAttributes(String directiveName, String attributeMask) {
		return getAttributeProposal(getAttributeTypesByName(directiveName, attributeMask, true));
	}

	private ArrayList<Element> getElementTypesByName(String name, boolean mask) {
//		KbPlugin.log("--> getElementTypesByName(String name, boolean mask)");
//		KbPlugin.log("    name = " + name);
//		KbPlugin.log("    mask = " + mask);

		ArrayList<Element> elements = new ArrayList<Element>();

		NodeList elementTypes = jspDirectiveSchema.getElementsByTagName(SchemaNodeFactory.ELEMENT_TYPE_NODE);
		for(int i=0; i<elementTypes.getLength(); i++) {
			Element elementType = (Element)elementTypes.item(i);
			String schemaDirectiveName = elementType.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);
			boolean match = mask?schemaDirectiveName.startsWith(name):schemaDirectiveName.equals(name);
			if(match) {
//				KbPlugin.log("    add element: " + elementType);
				elements.add(elementType);
			}
		}

//		KbPlugin.log("<-- getElementTypesByName()");
//		KbPlugin.log("    elements Size = " + elementType.size());

		return elements;
	}

	private ArrayList<Element> getAttributeTypesByName(String directiveName, String attributeName, boolean mask) {
		ArrayList<Element> attributes = new ArrayList<Element>();

		ArrayList elements = getElementTypesByName(directiveName, false);
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

	private ArrayList<KbProposal> getEnumeration(String directiveName, String attributeName, String valueMask) {
		ArrayList<KbProposal> enumeration = new ArrayList<KbProposal>();

		ArrayList attributeTypes = new ArrayList();
		attributeTypes = getAttributeTypesByName(directiveName, attributeName, false);
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

	private ArrayList<KbProposal> getDirectiveProposal(ArrayList elements) {
		return getProposal(elements, true);
	}

	private ArrayList<KbProposal> getAttributeProposal(ArrayList elements) {
		return getProposal(elements, false);
	}

	private ArrayList<KbProposal> getProposal(ArrayList elements, boolean directive) {
//		KbPlugin.log("--> getProposal(ArrayList elements, boolean directive)");
//		KbPlugin.log("    elemnets size = " + elements.size());
//		KbPlugin.log("    directive = " + directive);

		ArrayList<KbProposal> kbProposals = new ArrayList<KbProposal>();

		for(int i=0; i<elements.size(); i++) {
			Element element = (Element)elements.get(i);
			String label = element.getAttribute(SchemaNodeFactory.NAME_ATTRIBUTE);

			KbProposal proposal = new KbProposal();

			if(directive) {
				label = "%@ " + label;
				proposal.setLabel(label);

				String attributes = KbSchemaUtil.getRequaredAttributesAsString(element);

				proposal.setReplacementString(label + attributes);
				int position = proposal.getReplacementString().indexOf('"');
				if(position!=-1) {
					position ++;
				} else {
					position = proposal.getReplacementString().length();
				}
				proposal.setPosition(position);
				proposal.setIcon(KbIcon.JSP_DIRECTIVE);
			} else {
				proposal.setReplacementString(label);
				proposal.setLabel(label);
				if(KbSchemaUtil.checkRequaredAttribute(element)) {
					proposal.setIcon(KbIcon.JSP_DIRECTIVE_ATTRIBUTE);
				} else {
					proposal.setIcon(KbIcon.JSP_DIRECTIVE_ATTRIBUTE_OPTIONAL);
				}
			}
			proposal.setContextInfo(KbSchemaUtil.getDescription(element));

//			KbPlugin.log("    add kbProposals:\n" + proposal.toString());
//			KbPlugin.log("    for element: " + element);

			kbProposals.add(proposal);
		}

//		KbPlugin.log("<-- getProposal()");
//		KbPlugin.log("    kbProposals size = " + kbProposals.size());

		return kbProposals;
	}
}