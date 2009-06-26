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

import org.jboss.tools.common.model.ui.views.palette.PaletteTaglibInserter;

public class FileTagProposalLoader implements ITagProposalLoader {

	private static final Map<String,TagProposal[]> extensionMap = new HashMap<String,TagProposal[]>();
	
	static TagProposal[] IMG_TAG_PROPOSALS = new TagProposal[]{
		new TagProposal(
			DropURI.JSF_HTML_URI,
			"h", //$NON-NLS-1$
			"graphicImage", //$NON-NLS-1$
			new AbsoluteFilePathAttributeValueLoader("value","","") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		),
		new TagProposal(
			DropURI.HTML_4_0_URI,
			TagProposal.EMPTY_PREFIX,
			"img", //$NON-NLS-1$
			new AbsoluteFilePathAttributeValueLoader("src","","") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		),
		new TagProposal(
			DropURI.STRUTS_HTML_URI,
			"html", //$NON-NLS-1$
			"img", //$NON-NLS-1$
			new AbsoluteFilePathAttributeValueLoader("page","","") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		),
		new TagProposal(
				DropURI.SEAM_URI,
				"s", //$NON-NLS-1$
				"graphicImage", //$NON-NLS-1$
				new AbsoluteFilePathAttributeValueLoader("url","","") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			)
// yradtsevich: fix of JBIDE-3984: Exclude t:graphicImage option from Insert tag dialog 		
//		,
//		new TagProposal(
//			DropURI.TOMOHAWK_URI,
//			"t",
//			"graphicImage",
//			new AbsoluteFilePathAttributeValueLoader("url","","")
//		)
	};
	
	static TagProposal[] CSS_TAG_PROPOSALS = new TagProposal[]{
		new TagProposal(
			DropURI.HTML_4_0_URI,
			TagProposal.EMPTY_PREFIX,
			"link", //$NON-NLS-1$
			new CssLinkAttributeValueLoader("href") //$NON-NLS-1$
		),
	};
	
	static TagProposal JSP_INCLUDE = new TagProposal(
		DropURI.JSP_URI,
		"jsp", //$NON-NLS-1$
		"include", //$NON-NLS-1$
		new AbsoluteFilePathAttributeValueLoader("page","","")						 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	);
	
	static TagProposal JSP_FORWARD = new TagProposal(
		DropURI.JSP_URI,
		"jsp", //$NON-NLS-1$
		"forward", //$NON-NLS-1$
		new AbsoluteFilePathAttributeValueLoader("page","","") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	);
	
	static TagProposal UI_INCLUDE = new TagProposal(
		PaletteTaglibInserter.faceletUri,
		"ui", //$NON-NLS-1$
		"include", //$NON-NLS-1$
		new AbsoluteFilePathAttributeValueLoader("src","","")						 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	);
	
	static TagProposal S_DECORATE = new TagProposal(
			DropURI.SEAM_URI,
			"s", //$NON-NLS-1$
			"decorate", //$NON-NLS-1$
			new AbsoluteFilePathAttributeValueLoader("template","","")						 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		);
		
	static TagProposal[] PAGE_TAG_PROPOSALS = new TagProposal[]{
		JSP_INCLUDE,
		JSP_FORWARD
	};
	
	static TagProposal[] XHTML_PAGE_TAG_PROPOSALS = new TagProposal[]{
		JSP_INCLUDE,
		JSP_FORWARD,
		UI_INCLUDE,
		S_DECORATE
	};
	
	static {
		// There is the question here what store HTML or TLD will been asked about TagDescription    
		extensionMap.put("jpg", IMG_TAG_PROPOSALS); //$NON-NLS-1$

		extensionMap.put("jpeg", IMG_TAG_PROPOSALS); //$NON-NLS-1$

		extensionMap.put("gif",IMG_TAG_PROPOSALS); //$NON-NLS-1$
		
		extensionMap.put("bmp",IMG_TAG_PROPOSALS); //$NON-NLS-1$
		
		extensionMap.put("png",IMG_TAG_PROPOSALS); //$NON-NLS-1$
		
		extensionMap.put("jsp",PAGE_TAG_PROPOSALS); //$NON-NLS-1$
		extensionMap.put("html",PAGE_TAG_PROPOSALS); //$NON-NLS-1$
		extensionMap.put("xhtml",XHTML_PAGE_TAG_PROPOSALS); //$NON-NLS-1$

		extensionMap.put(
				"properties",new TagProposal[]{ //$NON-NLS-1$
					new TagProposal(
							DropURI.JSF_CORE_URI,
							"f", //$NON-NLS-1$
							"loadBundle", //$NON-NLS-1$
							new LoadBundleBaseNameAttributeValueLoader()
						)
					}
				);
		extensionMap.put("css",CSS_TAG_PROPOSALS); //$NON-NLS-1$
		extensionMap.put(
			"inc", new TagProposal[]{ //$NON-NLS-1$
				new TagProposal(
					DropURI.JSP_URI,
					"jsp", //$NON-NLS-1$
					"include", //$NON-NLS-1$
					new AbsoluteFilePathAttributeValueLoader("page","","")						 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				)
			}
		);
			
	}
	
	public static boolean isExtensionMapped(String extension) {
		return extension != null && extensionMap.containsKey(extension.toLowerCase());
	}
	
	public TagProposal[] getTagProposals(Object data) {
		String fileName = data.toString();
		String extension = fileName.substring(fileName.lastIndexOf(".")+1); //$NON-NLS-1$
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