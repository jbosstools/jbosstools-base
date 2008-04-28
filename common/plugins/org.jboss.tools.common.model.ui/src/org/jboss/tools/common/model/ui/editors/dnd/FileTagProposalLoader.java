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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.util.HashMap;
import java.util.Map;

public class FileTagProposalLoader implements ITagProposalLoader {

	private static final Map<String,TagProposal[]> extensionMap = new HashMap<String,TagProposal[]>();
	
	static TagProposal[] IMG_TAG_PROPOSALS = new TagProposal[]{
		new TagProposal(
			DropURI.JSF_HTML_URI,
			"h",
			"graphicImage",
			new AbsoluteFilePathAttributeValueLoader("value","","")
		),
		new TagProposal(
			DropURI.HTML_4_0_URI,
			TagProposal.EMPTY_PREFIX,
			"img",
			new AbsoluteFilePathAttributeValueLoader("src","","")
		),
		new TagProposal(
			DropURI.STRUTS_HTML_URI,
			"html",
			"img",
			new AbsoluteFilePathAttributeValueLoader("page","","")
		),
		new TagProposal(
			DropURI.TOMOHAWK_URI,
			"t",
			"graphicImage",
			new AbsoluteFilePathAttributeValueLoader("url","","")
		)
	};
	
	static TagProposal[] CSS_TAG_PROPOSALS = new TagProposal[]{
		new TagProposal(
			DropURI.HTML_4_0_URI,
			TagProposal.EMPTY_PREFIX,
			"link",
			new CssLinkAttributeValueLoader("href")
		),
	};
	
	static TagProposal[] PAGE_TAG_PROPOSALS = new TagProposal[]{
		new TagProposal(
			DropURI.JSP_URI,
			"jsp",
			"include",
			new AbsoluteFilePathAttributeValueLoader("page","","")						
		),
		new TagProposal(
			DropURI.JSP_URI,
			"jsp",
			"forward",
			new AbsoluteFilePathAttributeValueLoader("page","","")
		)
	};
	
	static {
		// There is the question here what store HTML or TLD will been asked about TagDescription    
		extensionMap.put("jpg", IMG_TAG_PROPOSALS);

		extensionMap.put("jpeg", IMG_TAG_PROPOSALS);

		extensionMap.put("gif",IMG_TAG_PROPOSALS);
		
		extensionMap.put("bmp",IMG_TAG_PROPOSALS);
		
		extensionMap.put("png",IMG_TAG_PROPOSALS);
		
		extensionMap.put("jsp",PAGE_TAG_PROPOSALS);
		extensionMap.put("html",PAGE_TAG_PROPOSALS);
		extensionMap.put("xhtml",PAGE_TAG_PROPOSALS);

		extensionMap.put(
				"properties",new TagProposal[]{
					new TagProposal(
							DropURI.JSF_CORE_URI,
							"f",
							"loadBundle",
							new LoadBundleBaseNameAttributeValueLoader()
						)
					}
				);
		extensionMap.put("css",CSS_TAG_PROPOSALS);
		extensionMap.put(
			"inc", new TagProposal[]{
				new TagProposal(
					DropURI.JSP_URI,
					"jsp",
					"include",
					new AbsoluteFilePathAttributeValueLoader("page","","")						
				)
			}
		);
			
	}
	
	public static boolean isExtensionMapped(String extension) {
		return extension != null && extensionMap.containsKey(extension.toLowerCase());
	}
	
	public TagProposal[] getTagProposals(Object data) {
		String fileName = data.toString();
		String extension = fileName.substring(fileName.lastIndexOf(".")+1);
		TagProposal[] tagProposals = (TagProposal[])extensionMap.get(extension.toLowerCase());
		if(tagProposals==null) {
			tagProposals = new TagProposal[0];
		}
		return tagProposals;
	}
	
	public boolean isTagProposalExists(Object data) {
		return true;
	}

	public static class ImageFileAttributesValuesLoader implements IAttributeValueLoader {

		public void fillTagAttributes(IDropWizardModel model) {
		}
		
	}
	
	public static class JspFileAttributesValuesLoader implements IAttributeValueLoader {

		public void fillTagAttributes(IDropWizardModel model) {
		}
	}
}