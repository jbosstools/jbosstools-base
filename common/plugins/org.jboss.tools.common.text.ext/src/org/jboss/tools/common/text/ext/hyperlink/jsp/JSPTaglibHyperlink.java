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

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.w3c.dom.Node;

import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.jst.web.tld.ITaglibMapping;
import org.jboss.tools.jst.web.tld.IWebProject;
import org.jboss.tools.jst.web.tld.WebProjectFactory;

/**
 * @author Jeremy
 */
public class JSPTaglibHyperlink extends AbstractHyperlink {
	
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
	
		try {
			XModelObject object = getFilename(region);
			if(object != null) FindObjectHelper.findModelObject(object, FindObjectHelper.IN_EDITOR_ONLY);
		} catch (Exception x) {
			// could not open editor
			openFileFailed();
		}
	}
	
	protected final String JAR_FILE_PROTOCOL = "jar:file:/";//$NON-NLS-1$
	
    /* (non-Javadoc)
     * @see com.ibm.sse.editor.hyperlink.AbstractHyperlink#openFileInEditor(java.lang.String)
     */
    protected void openFileInEditor(String fileString) {
        try {
	        if (fileString.startsWith(JAR_FILE_PROTOCOL)) {
				fileString = fileString.substring(JAR_FILE_PROTOCOL.length());
				IEditorInput jarEditorInput = createEditorInput(fileString);
				IEditorPart part = openFileInEditor(jarEditorInput,  fileString);
		        if (part == null) openFileFailed();
			} else {
				super.openFileInEditor(fileString);    
			}
        } catch (Exception x) {
        	openFileFailed();
        }
    }
    
	private XModelObject getFilename(IRegion region) {
		IFile file = getFile();
		XModel xModel = getXModel(file);
		if (xModel == null) return null;
		IStructuredModel model = null;
		try {	
			model = getModelManager().getExistingModelForRead(getDocument());
			IDOMDocument xmlDocument = (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;
			if (xmlDocument == null) return null;

			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());
			if (n instanceof IDOMAttr) n = ((IDOMAttr)n).getOwnerElement();
			if (!(n instanceof IDOMElement)) return null;
			if(!"jsp:directive.taglib".equals(n.getNodeName())) return null;

			IDOMElement taglib = (IDOMElement)n;
			
			String uri = taglib.getAttribute("uri");
			if (uri == null || uri.trim().length() == 0) return null;
			
			IWebProject wp = WebProjectFactory.instance.getWebProject(xModel);
			if (wp == null) return null;
			
			ITaglibMapping tm = wp.getTaglibMapping();
			if (tm == null) return null;
			return tm.getTaglibObject(uri);
		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error in obtaining file name from region", x);
			return null;
		} finally {
			if (model != null)	model.releaseFromRead();
		}
	}
	

	/*
	 * (non-Javadoc)
	 * 
	 * @seecom.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		IRegion region = getRegion(offset);
		return region;
	}
	
	private IRegion getRegion(int offset) {
		IStructuredModel model = null;
		try {	
			model = getModelManager().getExistingModelForRead(getDocument());
			IDOMDocument xmlDocument = (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			if (n instanceof IDOMAttr) n = ((IDOMAttr)n).getOwnerElement();
			if (!(n instanceof IDOMElement)) return null;
			if (!"jsp:directive.taglib".equals(n.getNodeName())) return null;

			IDOMElement taglib = (IDOMElement)n;
			
			final int taglibLength = taglib.getEndOffset() - taglib.getStartOffset();
			final int taglibOffset = taglib.getStartOffset();
			
			IRegion region = new IRegion () {
				public int getLength() {
					return taglibLength;
				}

				public int getOffset() {
					return taglibOffset;
				}
				
				public boolean equals(Object arg) {
					if (!(arg instanceof IRegion)) return false;
					IRegion region = (IRegion)arg;
					
					if (getOffset() != region.getOffset()) return false;
					if (getLength() != region.getLength()) return false;
					return true;
				}

			};
			return region;
		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error in obtaining region", x);
			return null;
		} finally {
			if (model != null)	model.releaseFromRead();
		}

	}
}