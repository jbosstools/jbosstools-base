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
package org.jboss.tools.common.model.ui.views.palette;

import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.xml.core.internal.document.DocumentImpl;
import org.eclipse.wst.xml.core.internal.document.ElementImpl;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.jst.web.tld.TaglibData;
import org.jboss.tools.jst.web.tld.VpeTaglibManager;
import org.jboss.tools.jst.web.tld.VpeTaglibManagerProvider;

public class PaletteTaglibInserter {
//	private static final String JSP_SOURCE_CONTENTTYPE_ID = "org.eclipse.jst.jsp.core.jspsource";
	private static final String JSP_SOURCE_ROOT_ELEMENT = "jsp:root";
	public static final String JSP_URI = "http://java.sun.com/JSP/Page";
//	private static final String HTML_SOURCE_CONTENTTYPE_ID = "org.eclipse.wst.html.core.htmlsource";
//	private static final String HTML_SOURCE_ROOT_ELEMENT = "html";
	public static final String faceletUri = "http://java.sun.com/jsf/facelets";
	
	private static final String TAGLIB_START = "<%@ taglib"; 

	public Properties inserTaglib(ISourceViewer v, Properties p) {
		IDocument d = v.getDocument();
		IStructuredModel model = null;

		if(!"true".equalsIgnoreCase(p.getProperty(PaletteInsertHelper.PROPOPERTY_ADD_TAGLIB)) ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI) == null ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI).length() == 0 ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI).equals(JSP_URI) ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX) == null ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX).length() == 0 ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_START_TEXT) == null ||
				p.getProperty(PaletteInsertHelper.PROPOPERTY_START_TEXT).startsWith(TAGLIB_START)) {
			return p;
		}

		try {
			model = StructuredModelManager.getModelManager().getExistingModelForRead(d);
			IDOMDocument xmlDocument = (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;
			if (xmlDocument == null) {
				return p;
			}
			Properties tl = getPrefixes(v);
			if(tl == null) tl = PaletteInsertHelper.getPrefixes(d.get());
			Element root = xmlDocument.getDocumentElement();
			// for xhtml facelets
			if (root != null && xmlDocument.getDoctype() != null /* && tagLibListConainsFacelet(tl)*/ ) {
				String publicId = xmlDocument.getDoctype().getPublicId();
				if (publicId!=null && publicId.toUpperCase().startsWith("-//W3C//DTD XHTML")) { // && root.getNodeName().equalsIgnoreCase(HTML_SOURCE_ROOT_ELEMENT)) {
					return checkTL(root, p, d);
				}
			// for jsp:root
			} else if (root != null && root.getNodeName().equals(JSP_SOURCE_ROOT_ELEMENT)) {
				return checkTL(root, p, d);
			}

			//for others
			String uri_p = p.getProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI);
			String defaultPrefix_p = p.getProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX);
			String lineDelimiter = PaletteInsertHelper.getLineDelimiter(d);
			StringBuffer tg = new StringBuffer(TAGLIB_START).append(" uri=\"").append(uri_p).append("\"").append(" prefix=\"").append(defaultPrefix_p).append("\"%>").append(lineDelimiter);

			if (tl != null && !tl.isEmpty()) {
				//If taglib already exist check the prefix if changed
				if (tl.containsKey(uri_p)) {
					if (!tl.get(uri_p).equals(defaultPrefix_p)) {
						p.setProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX, (String)tl.get(uri_p));
					}
				} else if(!tl.containsValue(defaultPrefix_p)) {
					if (checkplace(xmlDocument, d, "jsp:directive.taglib", tg, p, v) == false) {
						d.replace(0, 0, tg.toString());
						mouveFocusOnPage(p,v, tg.toString().length(), 0);
					}
				}
			} else if(xmlDocument instanceof DocumentImpl) {
					DocumentImpl docImpl = (DocumentImpl)xmlDocument;
					// Only for JSP
					if(docImpl.isJSPType()) {
						if (checkplace(xmlDocument, d, "jsp:directive.page", tg, p, v) == false) {
							d.replace(0, 0, tg.toString());
							mouveFocusOnPage(p,v, tg.toString().length(), 0);
						}
					}
				}
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		} finally {
			if (model != null)	model.releaseFromRead();
		}
		return p;
	}

//	private static boolean tagLibListConainsFacelet(List tagLibList) {
//		if (tagLibList != null && !tagLibList.isEmpty()) {
//			for (int i = 0; i < tagLibList.size(); i++) {
//				TaglibData tgld = (TaglibData)tagLibList.get(i);
//				if(faceletUri.equals(tgld.getUri())) {
//					return true;
//				}
//			}
//		}
//		return false;
//	}

	/*
	 * analyse source for taglib, return the list of taglib
	 */
	private static Properties getPrefixes(ISourceViewer viewer) {
		VpeTaglibManager tldManager = null;
		if((tldManager == null) && (viewer instanceof VpeTaglibManagerProvider)) {
			tldManager = ((VpeTaglibManagerProvider)viewer).getTaglibManager();
			if(tldManager != null) {
				List list = tldManager.getTagLibs();
				Properties p = new Properties();
				for (int i = 0; i < list.size(); i++) {
					TaglibData data = (TaglibData)list.get(i);
					p.setProperty(data.getUri(), data.getPrefix());
				}
				return p;
			}			
		}
		return null;
	}
	
	/*
	 * for jsp:root and html check the taglib if exist check the prefix else add the taglib
	 * with text formatting
	 */
	private static Properties checkTL(Element root, Properties p, IDocument d) {
		String uri_p = p.getProperty(PaletteInsertHelper.PROPOPERTY_TAGLIBRARY_URI);
		String defaultPrefix_p = p.getProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX);

		HashMap<String,String> map = new HashMap<String,String>();
		NamedNodeMap attrs = root.getAttributes();
		for (int j = 0; attrs != null && j < attrs.getLength(); j++) {
			Node a = attrs.item(j);
			String name = a.getNodeName();

			if (name.startsWith("xmlns:")) {
				map.put(a.getNodeValue(), name.substring("xmlns:".length()));
			}
		}

		if (map.containsKey(uri_p)) {
			if (!map.get(uri_p).equals(defaultPrefix_p)) {
				p.setProperty(PaletteInsertHelper.PROPOPERTY_DEFAULT_PREFIX, (String) map.get(uri_p));
			}
		} else if(!map.containsValue(defaultPrefix_p)) {
			StringBuffer attribute = new StringBuffer("xmlns:").append(defaultPrefix_p).append("=\"").append(uri_p).append("\""); 
			int so = ((IDOMElement)root).getStartOffset();
			int seo = ((IDOMElement)root).getStartEndOffset();
			try {
				String lineDelimiter = PaletteInsertHelper.getLineDelimiter(d);
				StringBuffer selectedSource = new StringBuffer().append(d.get(so, seo-so));
				int xmlns = selectedSource.indexOf("xmlns");
				attribute = new StringBuffer().append(createEmptyCharArray(xmlns)).append("xmlns:").append(defaultPrefix_p).append("=\"").append(uri_p).append("\"");
				if (d.getLineOffset(d.getLineOfOffset(so)) != so) {										
					attribute.insert(0, analyseSubstring(d.get(d.getLineOffset(d.getLineOfOffset(so)), so-d.getLineOffset(d.getLineOfOffset(so)))));
				}				
				if(xmlns>0) {
					attribute.insert(0, lineDelimiter);
				} else {
					attribute.insert(0, ' ');
				}
				selectedSource.insert(selectedSource.length()-1, attribute);
				d.replace(so, seo-so, selectedSource.toString());
			} catch (Exception t) {
				ModelUIPlugin.log("", t);
			}
		}
		return p;
	}

	private static char[] createEmptyCharArray(int n){
		if(n<1) {
			return new char[0];
		}
		char[] ca = new char[n];
		for (int i = 0; i < n; i++)
			ca[i]=' ';
		return ca;
	}

	private static void mouveFocusOnPage(Properties p, ISourceViewer v, int length, int pos){
		
		ISelectionProvider selProvider = (ISelectionProvider)p.get(PaletteInsertHelper.PROPOPERTY_SELECTION_PROVIDER);
		IDocument doc = v.getDocument();

		if (doc== null || selProvider == null) return;
		
		ITextSelection selection = (ITextSelection)selProvider.getSelection();
		if (selection.getOffset() == 0) {			
			 v.setSelectedRange(length,0);
			 p.put(PaletteInsertHelper.PROPOPERTY_SELECTION_PROVIDER,v.getSelectionProvider());
		}
		else 
		if (selection.getOffset() == pos ){
			v.setSelectedRange(length, 0);
			p.put(PaletteInsertHelper.PROPOPERTY_SELECTION_PROVIDER,v.getSelectionProvider());
		}
	}

	/*
	 * analyse the space between the left corner and the start offset o the text
	 */ 
	private static StringBuffer analyseSubstring(String str){
		StringBuffer st = new StringBuffer().append(str);		
		for (int i = 0; i < st.length(); i++) {
			if (st.charAt(i) != ' ' && st.charAt(i) !='\t' ) {
				st.setCharAt(i, ' ');
			}
		}
		return st;		
	}

	private static boolean checkplace(IDOMDocument xmlDocument, IDocument d, String st, StringBuffer tg, Properties p, ISourceViewer v) throws Exception {
		NodeList nl = xmlDocument.getChildNodes();
		boolean docType = false;
		IndexedRegion irdt = null;

		if (xmlDocument.getDoctype() != null) {
			docType = true;
			String publicId = xmlDocument.getDoctype().getPublicId();
			if (publicId!=null && publicId.toUpperCase().startsWith("-//W3C//DTD HTML")) {
				irdt = (xmlDocument.getDoctype() instanceof IndexedRegion) ?
						(IndexedRegion)xmlDocument.getDoctype(): null;
			}
		}

		if (nl != null && nl.getLength() != 0) {
			for (int i=0; i < nl.getLength(); i++) {
				Node n = nl.item(i);
				//fing the first taglib to insert before
				if (n.getNodeName().equals(st) && st.equals("jsp:directive.taglib")) {
					//calculate the space between taglib and left page corner
					int so = ((ElementImpl)n).getStartOffset();
					//taglib is at left corner 
					if (d.getLineOffset(d.getLineOfOffset(so)) == so) {
						d.replace(so, 0, tg.toString());
					} else {
						StringBuffer left = new StringBuffer().
						append(analyseSubstring(d.get(d.getLineOffset(d.getLineOfOffset(so)), so-d.getLineOffset(d.getLineOfOffset(so)))));
						tg.insert(tg.length(), left);
						d.replace(so, 0, tg.toString());
					}
					return true;
				}
				if ((n.getNodeName().equals(st) && st.equals("jsp:directive.page"))	) {
					tg.delete(tg.lastIndexOf(PaletteInsertHelper.getLineDelimiter(d)), tg.length());
					int so = ((ElementImpl)n).getStartOffset();
					int eo = ((ElementImpl)n).getEndStartOffset();
					StringBuffer tgleft = new StringBuffer().append(PaletteInsertHelper.getLineDelimiter(d));
					if (d.getLineOffset(d.getLineOfOffset(so)) == so) {
						tgleft.append(tg);
						d.replace(eo, 0, tgleft.toString());
						mouveFocusOnPage(p,v, eo + tgleft.length(), eo);
					} else {
						tgleft.append(analyseSubstring(d.get(d.getLineOffset(d.getLineOfOffset(so)), so-d.getLineOffset(d.getLineOfOffset(so)))));
						tgleft.append(tg);
						d.replace(eo, 0, tgleft.toString());
						mouveFocusOnPage(p,v, eo + tgleft.length(), eo);
					}
					return true;
				}
				if (docType && irdt != null) {
					tg.delete(tg.lastIndexOf(PaletteInsertHelper.getLineDelimiter(d)), tg.length());
					int so = irdt.getStartOffset();
					int eo = irdt.getEndOffset();
					StringBuffer tgleft = new StringBuffer().append(PaletteInsertHelper.getLineDelimiter(d));
					if (d.getLineOffset(d.getLineOfOffset(so)) == so) {
						tgleft.append(tg);
						d.replace(eo, 0, tgleft.toString());
						mouveFocusOnPage(p,v, eo + tgleft.length(), eo);
					} else {
						tgleft.append(analyseSubstring(d.get(d.getLineOffset(d.getLineOfOffset(so)), so-d.getLineOffset(d.getLineOfOffset(so)))));
						tgleft.append(tg);
						d.replace(eo, 0, tgleft.toString());
						mouveFocusOnPage(p,v, eo + tgleft.length(), eo);
					}
					return true;
				}
			}
		}
		return false;
	}	
}
