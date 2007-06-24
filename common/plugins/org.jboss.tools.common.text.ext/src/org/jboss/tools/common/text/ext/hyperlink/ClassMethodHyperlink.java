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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;

/**
 * @author Jeremy
 */
public abstract class ClassMethodHyperlink extends AbstractHyperlink {
	protected abstract String getMethodName(IRegion region);

	protected abstract String getClassName(IRegion region);

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
		
		try {
			String className = getClassName(region);
			String methodName = getMethodName(region);
			
			IJavaElement classMethod = null;
			if (className != null && className.trim().length() > 0) {
				classMethod = searchForClassMethod(className, methodName);
			}

			if (classMethod != null) {
				IEditorPart part = JavaUI.openInEditor(classMethod);
				if (part != null) {
					if (classMethod != null)
						JavaUI.revealInEditor(part, classMethod);
				}
				else {
					// could not open editor
					openFileFailed();
				}
			} else {
				openFileFailed();
			}
		} catch (Exception x) {
			openFileFailed();
		}
	}
	
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		IRegion region = getRegion(offset);
		return region;
	}

	private IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Attr || n instanceof Text)) return null;
			
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);

			if (start < 0 || start > offset || end < offset) return null;

			String text = getDocument().get(start, end - start);
			StringBuffer sb = new StringBuffer(text);

			//find start and end of path property
			int bStart = 0;
			int bEnd = text.length() - 1;

			while (bStart < bEnd && (Character.isWhitespace(sb.charAt(bStart)) 
					|| sb.charAt(bStart) == '\"' || sb.charAt(bStart) == '\"')) { 
				bStart++;
			}
			while (bEnd > bStart && (Character.isWhitespace(sb.charAt(bEnd)) 
					|| sb.charAt(bEnd) == '\"' || sb.charAt(bEnd) == '\"')) { 
				bEnd--;
			}
			bEnd++;

			int propStart = bStart + start;
			int propLength = bEnd - bStart;
			if (propStart > offset || propStart + propLength < offset) return null;
			IRegion region = new HyperlinkRegion(propStart, propLength);
			return region;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	protected IJavaElement searchForClassMethod(IJavaProject javaProject, String className, String methodName) {
		try {
//		 Get the search pattern
	    SearchPattern pattern = SearchPattern.createPattern(className + "." + methodName, IJavaSearchConstants.METHOD, IJavaSearchConstants.DECLARATIONS, SearchPattern.R_EXACT_MATCH | SearchPattern.R_CASE_SENSITIVE);
	    
	    // Get the search scope
	    IJavaSearchScope scope = SearchEngine.createJavaSearchScope(new IJavaElement[] { javaProject });

	    final List<SearchMatch> matches = new ArrayList<SearchMatch>();
	    // Get the search requestor
	    SearchRequestor requestor = new SearchRequestor() {
			public void acceptSearchMatch(SearchMatch match) throws CoreException {
				matches.add(match);
			}
	    };

	    // Search
	    SearchEngine searchEngine = new SearchEngine();
	    try {
	    	searchEngine.search(pattern, new SearchParticipant[] {SearchEngine.getDefaultSearchParticipant()}, scope, requestor, null);
	    } catch (CoreException ex) {
	    	//ignore
	    }
	    for (Iterator i = matches.iterator(); i != null && i.hasNext();) {
	    	return (IJavaElement)((SearchMatch)i.next()).getElement();
	    }
	    return null;
		} finally {
			//
		}
	}

	protected IJavaElement searchForClassMethod(String className, String methodName) {
		IFile documentFile = getFile();
		
		try {	
			IProject project = null;
			if (documentFile == null) {
				IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
				IEditorPart activeEditorPart = workbenchPage.getActiveEditor();
				IEditorInput editorInput = activeEditorPart.getEditorInput();
				if (editorInput instanceof IStorageEditorInput) {
					IStorageEditorInput moeInput = (IStorageEditorInput)editorInput;
					IPath p = moeInput.getStorage().getFullPath();
					String s0 = p.segment(0);
					project = ExtensionsPlugin.getWorkspace().getRoot().getProject(s0); 
				}
			} else {
				project = documentFile.getProject();
			}
			
			if(project == null || !project.isOpen()) 
				return null;
			if(!project.hasNature(JavaCore.NATURE_ID)) 
				return null;
			IJavaProject javaProject = JavaCore.create(project);
			return searchForClassMethod(javaProject, className, methodName);

		} catch (Exception x) {
			ExtensionsPlugin.log("Error while looking for method " + methodName + " of class " + className, x);
			return null;
		}

	}

	protected String getAttributeValue(Node node, String attrName) {
		try {
			Attr attr = (Attr)node.getAttributes().getNamedItem(attrName);
			return Utils.getTrimmedValue(getDocument(), attr);
		} catch (Exception x) {
			//ignore
			return null;
		}
	}
	
	protected String getAttributeValue(IRegion region, String attrName) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, region.getOffset());

			if (n == null || !(n instanceof Attr)) return null;
			
			Node node = ((Attr)n).getOwnerElement();
			
			Attr attr = (Attr)node.getAttributes().getNamedItem(attrName);
			
			return Utils.getTrimmedValue(getDocument(), attr);
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

}