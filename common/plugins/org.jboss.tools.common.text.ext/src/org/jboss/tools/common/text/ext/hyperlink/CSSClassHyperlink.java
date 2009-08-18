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
package org.jboss.tools.common.text.ext.hyperlink;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.ide.IDE;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMText;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.CSSTextScanner;
import org.jboss.tools.common.text.ext.util.RegionHolder;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.StructuredSelectionHelper;
import org.jboss.tools.common.text.ext.util.TextScanner;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/*
 * Created on 26.01.2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author Jeremy
 */
public class CSSClassHyperlink extends AbstractHyperlink {

	// Qualifiers for CSSList
	public static final String QUALIFIER = "CSSListForm"; //$NON-NLS-1$
	public static final String LOCAL_NAME = "CSSList"; //$NON-NLS-1$

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
	
		try {
			RegionHolder holder = getStyleHolder(getStyleName(region));
			if (holder != null) {
				if (holder.file != null) {
					IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
					IEditorPart part = IDE.openEditor(workbenchPage,holder.file,true);
					if (part == null) {
						openFileFailed();
						return;
					}
					StructuredSelectionHelper.setSelectionAndReveal(part, holder.region);
				} else {
					StructuredSelectionHelper.setSelectionAndRevealInActiveEditor(holder.region);
				}
			} else {
				openFileFailed();
			}
			
		} catch (CoreException x) {
			// could not open editor
			openFileFailed();
		}
	}
	
	private String getStyleName(IRegion region) {
		try {
			return Utils.trimQuotes(getDocument().get(region.getOffset(), region.getLength()));
		} catch (BadLocationException x) {
			//ignore
			return null;
		}
	}
	
	IRegion fLastRegion = null;
	
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}
	
	private RegionHolder getStyleHolder (String styleName) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			IFile documentFile = smw.getFile();
			if (documentFile == null)
				return null;
			
			IProject project = documentFile.getProject();

			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			List styleLinks = findStyleLinks(xmlDocument.getChildNodes());
			
			if (styleLinks == null || styleLinks.size() == 0) return null;
			boolean doSearchInProjectCSSList = false;
			for (int i = styleLinks.size() - 1; i >= 0; i--) {
				if (styleLinks.get(i) instanceof Attr) {
					Attr hrefAttr = (Attr)styleLinks.get(i);
					String fileName = Utils.trimQuotes(hrefAttr.getValue());
					if (fileName != null && fileName.length() > 0) {
						IFile file = getFileFromProject(fileName);
						if (file == null || !file.exists()) doSearchInProjectCSSList = true;
						IRegion region = findStyleRegion(file, styleName);
						if (region != null) {
							return new RegionHolder(file, region);
						}
					}
				} else if (styleLinks.get(i) instanceof IDOMElement) {
					IDOMElement styleElement = (IDOMElement)styleLinks.get(i);
					IRegion region = findStyleRegion(styleElement, styleName);
					
					if ( region != null) {
						return new RegionHolder(region);
					} else {
						//"Style region not found"
					}
				}
			}
			
			if (doSearchInProjectCSSList) {
				String cssList = project.getPersistentProperty(new QualifiedName(QUALIFIER, LOCAL_NAME));
				if (cssList != null) {
					StringTokenizer st = new StringTokenizer(cssList, ","); //$NON-NLS-1$
					while (st.hasMoreTokens()) {
						String fileName = st.nextToken().trim();
						IPath path = new Path(fileName).removeFirstSegments(2);
						fileName = path.toString();
						if (fileName != null && fileName.length() > 0) {
							IFile file = getFileFromProject(fileName);
							IRegion region = findStyleRegion(file, styleName);
							if (region != null) {
								return new RegionHolder(file, region);
							}
						}
					}
				}
			}
			
			return null;
		} catch (CoreException x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	private IRegion findStyleRegion(IDOMElement styleElement, String styleName) {
		if (styleElement == null || styleName == null || styleName.length() == 0) return null;
		
		// get all the XMLText regions and combine them into the string
		// Then parse that string finding the style class
		
		if (!(styleElement.getFirstChild() instanceof IDOMText)) return null;
		
		IDOMText styleText = (IDOMText)styleElement.getFirstChild();
		String text = styleText.getData();
		if (text == null) return null;
		
		int startOffset = styleText.getStartOffset();
		IRegion region = null;
		StringReader reader = new StringReader (text);
		try {
			CSSTextScanner scanner = new CSSTextScanner(reader);
			IToken token = scanner.nextToken();
			while (token != null && !token.isEOF()) {
				if (token instanceof TextScanner.TextToken) {
					TextScanner.TextToken tToken = (TextScanner.TextToken)token;
					if (CSSTextScanner.CSS_CLASS_NAME.equals(tToken.getType())) {
						String name = tToken.getText();
						if (("." + styleName).equalsIgnoreCase(name)) { //$NON-NLS-1$
							final int offset = startOffset + scanner.getTokenOffset();
							final int length = scanner.getTokenLength();
							return new Region (offset,length);
						}
					} else if (CSSTextScanner.CSS_CLASS_BODY.equals(tToken.getType())) {
//						String body = tToken.getText();
					}
				} 
				
				token = scanner.nextToken();
			}
		} finally {
				reader.close();
		}
		return null;
	}
	
	private IRegion findStyleRegion(IFile file, String styleName) {
		if (file == null || !file.exists()) return null;
		if (styleName == null || styleName.length() == 0) return null;
		InputStream stream = null;
		try {
			stream = file.getContents(true);
			CSSTextScanner scanner = new CSSTextScanner(stream);

			IToken token = scanner.nextToken();
			while (token != null && !token.isEOF()) {
				if (token instanceof TextScanner.TextToken) {
					TextScanner.TextToken tToken = (TextScanner.TextToken)token;
					if (CSSTextScanner.CSS_CLASS_NAME.equals(tToken.getType())) {
						String name = tToken.getText();
						if (("." + styleName).equalsIgnoreCase(name)) { //$NON-NLS-1$
							final int offset = scanner.getTokenOffset();
							final int length = scanner.getTokenLength();
							return new Region (offset,length);
						}
					} else if (CSSTextScanner.CSS_CLASS_BODY.equals(tToken.getType())) {
//						String body = tToken.getText();
//						"CSS_CLASS_BODY Token: [" + body + "]"
					}
				} 
				
				token = scanner.nextToken();
			}
		} catch (CoreException x) {
			ExtensionsPlugin.getPluginLog().logError("Error while looking for style region ", x); //$NON-NLS-1$
		} finally {
			try {
				if(stream!=null) {
					stream.close();
				}
			} catch (IOException x) {
				//ignore
			}
		}
		return null;
	}
	
	private List<Node> findStyleLinks (NodeList list) {
		List<Node> styleLinks = new ArrayList<Node>();
			for (int i = 0; list != null && i < list.getLength(); i++) {

					IDOMNode element = (IDOMNode)list.item(i);
					//String axis = JSPRootHyperlinkPartitioner.computeAxis(getDocument(), element.getStartOffset());
					//axis  = axis.toLowerCase();
					
					if ("link".equals(element.getNodeName())) { //$NON-NLS-1$
						Node relAttr = element.getAttributes().getNamedItem("rel"); //$NON-NLS-1$
						if (relAttr != null) {
							String val = relAttr.getNodeValue().toLowerCase();
							if ("stylesheet".equalsIgnoreCase(val) || "\"stylesheet\"".equalsIgnoreCase(val)) { //$NON-NLS-1$ //$NON-NLS-2$
								Node hrefAttr = element.getAttributes().getNamedItem("href"); //$NON-NLS-1$
								if (hrefAttr != null) {
									styleLinks.add(hrefAttr);
								}
							}
						}
					}
					
					if ("link".equals(element.getNodeName())) { //$NON-NLS-1$
//						String value = element.getNodeValue();
						styleLinks.add(element);
					}
					
					if (element.hasChildNodes()) {
						List<Node> add = findStyleLinks(element.getChildNodes());
						if (add != null) 
							styleLinks.addAll(add);
					}
			}
			return styleLinks;
	}
	
	protected IRegion getRegion (int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			
			if (n == null || !(n instanceof Attr)) return null;
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			if (start > offset) return null;

			String attrText = getDocument().get(start, end - start);

			StringBuffer sb = new StringBuffer(attrText);
			//find start of bean property
			int bStart = offset - start;
			while (bStart >= 0) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bStart)) && 
						sb.charAt(bStart) != '_' && sb.charAt(bStart) != '-' &&
						sb.charAt(bStart) != '.') {
					bStart++;
					break;
				}
			
				if (bStart == 0) break;
				bStart--;
			}
			// find end of bean property
			int bEnd = offset - start;
			while (bEnd < sb.length()) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bEnd)) && 
						sb.charAt(bEnd) != '_' && sb.charAt(bEnd) != '-' &&
						sb.charAt(bEnd) != '.')
					break;
				bEnd++;
			}
			
			final int propStart = bStart + start;
			final int propLength = bEnd - bStart;
			
			if (propStart > offset || propStart + propLength < offset) return null;
			return new Region (propStart,propLength);
		} catch (BadLocationException x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see IHyperlink#getHyperlinkText()
	 */
	public String getHyperlinkText() {
		String styleName = getStyleName(fLastRegion);
		if (styleName == null)
			return  MessageFormat.format(Messages.OpenA, Messages.CSSStyle);
		
		return MessageFormat.format(Messages.OpenCSSStyle, styleName);
	}

}