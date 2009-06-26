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
package org.jboss.tools.common.text.ext.hyperlink.jsp;

import java.text.MessageFormat;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.jst.web.tld.ITaglibMapping;
import org.jboss.tools.jst.web.tld.IWebProject;
import org.jboss.tools.jst.web.tld.WebProjectFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * @author Jeremy
 */
public class JSPXmlNsHyperlink extends AbstractHyperlink {
	
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
		XModelObject object = getFilename(region);
		if(object != null) FindObjectHelper.findModelObject(object, FindObjectHelper.IN_EDITOR_ONLY);
		else {
			openFileFailed();
		}
	}
	
	protected final String JAR_FILE_PROTOCOL = "jar:file:/";//$NON-NLS-1$
	
    /**
     * @see com.ibm.sse.editor.hyperlink.AbstractHyperlink#openFileInEditor(java.lang.String)
     */
    protected void openFileInEditor(String fileString) {
	        if (fileString.startsWith(JAR_FILE_PROTOCOL)) {
				fileString = fileString.substring(JAR_FILE_PROTOCOL.length());
				IEditorInput jarEditorInput = createEditorInput(fileString);
		        IEditorPart part = openFileInEditor(jarEditorInput,  fileString);
		        if (part == null) openFileFailed();
			} else {
				super.openFileInEditor(fileString);    
			}
    }
    
	private XModelObject getFilename(IRegion region) {
		IFile file = getFile();
		XModel xModel = getXModel(file);
		if (xModel == null) return null;

		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());
			if (!(n instanceof IDOMAttr)) return null; 
			Attr xmlnsAttr = (Attr)n;
			if (xmlnsAttr.getName() == null || !xmlnsAttr.getName().startsWith("xmlns:")) return null; //$NON-NLS-1$
			Element rootElem = (Element)xmlnsAttr.getOwnerElement();
			if (!(rootElem.getNodeName().equals("jsp:root") || rootElem.getNodeName().equalsIgnoreCase("html"))) return null; //$NON-NLS-1$ //$NON-NLS-2$

			String uri = xmlnsAttr.getValue();
			if (uri == null || uri.trim().length() == 0) return null;
			
			IWebProject wp = WebProjectFactory.instance.getWebProject(xModel);
			if (wp == null) return null;
			
			ITaglibMapping tm = wp.getTaglibMapping();
			if (tm == null) return null;
			return tm.getTaglibObject(uri);
		} finally {
			smw.dispose();
		}
	}

	private String getUri(IRegion region) {
		IFile file = getFile();
		XModel xModel = getXModel(file);
		if (xModel == null) return null;

		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());
			if (!(n instanceof IDOMAttr)) return null; 
			Attr xmlnsAttr = (Attr)n;
			if (xmlnsAttr.getName() == null || !xmlnsAttr.getName().startsWith("xmlns:")) return null; //$NON-NLS-1$
			Element rootElem = (Element)xmlnsAttr.getOwnerElement();
			if (!(rootElem.getNodeName().equals("jsp:root") || rootElem.getNodeName().equalsIgnoreCase("html"))) return null; //$NON-NLS-1$ //$NON-NLS-2$

			String uri = xmlnsAttr.getValue();
			if (uri == null || uri.trim().length() == 0) return null;
			
			return uri;
		} finally {
			smw.dispose();
		}
	}


	IRegion fLastRegion = null;
	/** 
	 * @seecom.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}
	
	private IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (!(n instanceof IDOMAttr)) return null; 
			IDOMAttr xmlnsAttr = (IDOMAttr)n;
			if (xmlnsAttr.getName() == null || !xmlnsAttr.getName().startsWith("xmlns:")) return null; //$NON-NLS-1$
			Element rootElem = xmlnsAttr.getOwnerElement();
			if (!(rootElem.getNodeName().equals("jsp:root") || rootElem.getNodeName().equalsIgnoreCase("html"))) return null; //$NON-NLS-1$ //$NON-NLS-2$

			final int taglibLength = xmlnsAttr.getValueRegionText().length();
			final int taglibOffset = xmlnsAttr.getValueRegionStartOffset();

			return new Region(taglibOffset,taglibLength);
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
		String uri = getUri(fLastRegion);
		if (uri == null)
			return  MessageFormat.format(Messages.NotFound, "URI"); //$NON-NLS-1$
		
		return MessageFormat.format(Messages.Open, uri);
	}
}