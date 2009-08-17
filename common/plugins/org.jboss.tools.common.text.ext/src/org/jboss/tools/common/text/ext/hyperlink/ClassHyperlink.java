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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.core.JarEntryResource;
import org.eclipse.jdt.internal.core.JarPackageFragmentRoot;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.xpl.Messages;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * @author Jeremy
 */
public class ClassHyperlink extends AbstractHyperlink {

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
	
		try {
			IJavaElement element = null;
			IEditorPart part = null;
			String className = getClassName(region);
			
			if (className != null && className.trim().length() > 0)
				element = searchForClass(className);

			if (element != null)
				part = JavaUI.openInEditor(element);
			
			if (part != null && element != null)
					JavaUI.revealInEditor(part, element);
			else {
				// could not open editor
				openFileFailed();
			}
		} catch (CoreException x) {
			// could not open editor
			openFileFailed();
		}
	}

	private String getClassName(IRegion region) {
		try {
			return getDocument().get(region.getOffset(), region.getLength());
		} catch (BadLocationException x) {
			// Ignore
			return null;
		} finally {
		}
	}
	
	private IJavaElement searchForClass(IJavaProject javaProject, String className) throws JavaModelException {
//		 Get the search pattern
	    SearchPattern pattern = SearchPattern.createPattern(className, IJavaSearchConstants.TYPE, IJavaSearchConstants.DECLARATIONS, SearchPattern.R_EXACT_MATCH | SearchPattern.R_CASE_SENSITIVE);
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
	    	// Ignore
//	    	ExtensionsPlugin.log(ex);
	    }
	    for (Iterator i = matches.iterator(); i != null && i.hasNext();) {
	    	IJavaElement element = (IJavaElement)((SearchMatch)i.next()).getElement();
	    	String classQualifiedName = getQualifiedClassName(element);
	    	if (className.equals(classQualifiedName)) 
	    		return element;
	    }
	    return javaProject.findType(className, new NullProgressMonitor());
	}

	private String getQualifiedClassName(IJavaElement element) {
		if(element instanceof IType) {
			return ((IType)element).getFullyQualifiedName('.');
		}
		return null;
	}
	
	private IJavaElement searchForClass(final String className) {
		IFile documentFile = getFile();
		
		try {	
			
			IProject project = null;
			if (documentFile == null) {
				IWorkbenchPage workbenchPage = ExtensionsPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage();
				IEditorPart activeEditorPart = workbenchPage.getActiveEditor();
				IEditorInput editorInput = activeEditorPart.getEditorInput();
				//added by Maksim Areshkau, fix for https://jira.jboss.org/jira/browse/JBIDE-4638
				//in this code we looking for java resource if editor has been opened in resource which packed into jar file
				if (editorInput instanceof JarEntryEditorInput) {
					JarEntryEditorInput jarEntryEditorInput = (JarEntryEditorInput) editorInput;
					JarEntryResource jarEntryFile = (JarEntryResource) jarEntryEditorInput.getStorage();
					Object parent = jarEntryFile.getParent();
					while( parent instanceof JarEntryResource) {
						parent = ((JarEntryResource)parent).getParent();
					}
					if( (parent instanceof JarPackageFragmentRoot) &&
							(((JarPackageFragmentRoot)parent).getParent() instanceof IJavaProject)) {
						return searchForClass(((IJavaProject) ((JarPackageFragmentRoot)parent).getParent()), className);
					}
					
				} else  if (editorInput instanceof IStorageEditorInput) {
					IStorageEditorInput moeInput = (IStorageEditorInput)editorInput;
					IPath p = moeInput.getStorage().getFullPath();
					String s0 = p.segment(0);
					project = ResourcesPlugin.getWorkspace().getRoot().getProject(s0); 
				}
			} else {
				project = documentFile.getProject();
			}
			
			if(project == null || !project.isOpen()) 
				return null;
			if(!project.hasNature(JavaCore.NATURE_ID)) 
				return null;
			IJavaProject javaProject = JavaCore.create(project);
			return searchForClass(javaProject, className);

		} catch (CoreException x) {
			ExtensionsPlugin.getPluginLog().logError("Error while looking for class " + className, x); //$NON-NLS-1$
			return null;
		}
	}

	IRegion fLastRegion = null;
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		fLastRegion = getRegion(offset);
		return fLastRegion;
	}

	public IRegion getRegion (int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);
			
			if (n == null || !(n instanceof Text || n instanceof Attr)) return null;
			
			String text = null;
			int bStart = 0;
			int bEnd = 0;
			
			if (n instanceof Text) {
				int start = Utils.getValueStart(n);
				int end = Utils.getValueEnd(n);
				if (start < 0 || start > offset) return null;
	
				text = getDocument().get(start, end - start);
				bStart = offset - start;
				bEnd = offset - start;
			} else if (n instanceof Attr) {
				IDOMAttr attr = (IDOMAttr)n;
				int start = Utils.getValueStart(n);
				if(start < 0) return null;

				text = getDocument().get(start, 
						attr.getValueRegionText().length());
				bStart = offset - start;
				bEnd = offset - start;
			}
			StringBuffer sb = new StringBuffer(text);

			while (bStart >= 0) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bStart)) &&
						sb.charAt(bStart) != '.' && sb.charAt(bStart) != '_') {
					bStart++;
					break;
				}
			
				if (bStart == 0) break;
				bStart--;
			}

			while (bEnd < sb.length()) { 
				if (!Character.isJavaIdentifierPart(sb.charAt(bEnd)) &&
						sb.charAt(bEnd) != '.' && sb.charAt(bEnd) != '_') {
					break;
				}
				bEnd++;
			}
			
			final int propStart = bStart + Utils.getValueStart(n);
			final int propLength = bEnd - bStart;
			
			if (propStart > offset || propStart + propLength < offset) return null;
			
			return new Region(propStart,propLength);			
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
		String className = getClassName(fLastRegion);
		if (className == null)
			return  MessageFormat.format(Messages.OpenA, Messages.Class);
		
		return MessageFormat.format(Messages.OpenClass, className);
	}

}