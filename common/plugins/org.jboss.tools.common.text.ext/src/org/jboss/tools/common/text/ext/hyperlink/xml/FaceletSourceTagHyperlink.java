/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xml;

import java.util.zip.ZipEntry;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.core.JarPackageFragmentRoot;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.LinkHyperlink;


/**
 * @author mareshkau
 *
 */
public class FaceletSourceTagHyperlink extends LinkHyperlink{


	@Override
	protected void doHyperlink(IRegion region) {
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart editorPart = page.getActiveEditor();
		// if we open taglib definition in jar file
		if( editorPart.getEditorInput() instanceof JarEntryEditorInput) {
			String fileToOpenName =getFilePath(region);
			if(fileToOpenName!=null) {
				JarEntryEditorInput currentEditorInput =  (JarEntryEditorInput) editorPart.getEditorInput();
				//remove whitespaces and first '/'
				fileToOpenName = fileToOpenName.trim();
				if(fileToOpenName.indexOf('/')==0) {
					fileToOpenName=fileToOpenName.substring(1);
				}
				JarEntryFile fileToOpen =  new JarEntryFile(fileToOpenName);
				fileToOpen.setParent(((JarEntryFile)currentEditorInput.getStorage()).getPackageFragmentRoot());
				JarEntryEditorInput  editorInputToOpenEditor= new JarEntryEditorInput(fileToOpen);
				IEditorPart openedEditor = openFileInEditor(editorInputToOpenEditor, 
						fileToOpen.getName());
				if(openedEditor==null) {
					openFileFailed();
				}
			}
			return;
		} 
		else if(editorPart.getEditorInput() instanceof IStorageEditorInput) {
			IStorageEditorInput modelObjectStorageEditorInput = 
				(IStorageEditorInput)	editorPart.getEditorInput(); 
			try {
				
				IStorage storage = modelObjectStorageEditorInput.getStorage();
				if(storage!=null && getFilePath(region)!=null) {
					if(openOnFromModelEditorIntup(storage.getFullPath(), getFilePath(region))){
						return;
					}
				}
			} catch (CoreException e) {
				ExtensionsPlugin.getDefault().logError(e);
			}
		} 
		super.doHyperlink(region);
	}
	/**
	 * 
	 * @param xmodelPaht it's path to file in jar archive on which we are trying to make open on
	 * Example 'JBIDE3247/FileSystems/lib-mydomain.jar/META-INF/custom.taglib.xml'
	 * @param fileToOpen jar archive relative name for which we are trying to make open on
	 * Example '/inputCurrency.xhtml's
	 */
	private boolean openOnFromModelEditorIntup(IPath xmodelPath, String fileToOpen) {
		if(xmodelPath==null || fileToOpen==null) {
			return false;
		}
		String projectName = xmodelPath.segment(0);
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		JarEntryEditorInput  editorInputToOpenEditor=seachResourceInClassPath(project,new Path(fileToOpen.substring(1)),xmodelPath);
		if(editorInputToOpenEditor==null) {
			return false;
		}
		IEditorPart openedEditor = openFileInEditor(editorInputToOpenEditor, 
				editorInputToOpenEditor.getName());
		if(openedEditor==null) {
			openFileFailed();
			return false;
		} 
		return true;
	}
	/**
	 * 
	 * @param project project from which we are going to open File
	 * @param classPathResource resource relativy to class path
	 * @param xmodelPath xmodel path to file
	 * @return
	 */
    private static JarEntryEditorInput  seachResourceInClassPath(IProject project, IPath classPathResource, IPath xmodelPath) {
		IJavaProject javaProject = JavaCore.create(project);
		try {
			for (IPackageFragmentRoot fragmentRoot : javaProject.getAllPackageFragmentRoots()) {
				if(fragmentRoot instanceof JarPackageFragmentRoot) {
					JarPackageFragmentRoot jarPackageFragmentRoot = (JarPackageFragmentRoot) fragmentRoot;
					ZipEntry zipEntry = jarPackageFragmentRoot.getJar().getEntry(classPathResource.toString());
					//the name of jar file shoul be in xmodel path
					//for now if model have following
					if(zipEntry!=null&&xmodelPath.toString().contains(fragmentRoot.getElementName())){
						JarEntryFile fileInJar = new JarEntryFile(classPathResource.lastSegment());									
						Object parent = null;
						String parentName =classPathResource.removeLastSegments(1).toString();
						if(parentName.length()==0) {
							parent = jarPackageFragmentRoot;
						}else{	
							parent = jarPackageFragmentRoot.getPackageFragment(classPathResource.removeLastSegments(1).toString());
						}
						fileInJar.setParent(parent);
						JarEntryEditorInput jarEditorInput = new JarEntryEditorInput(fileInJar);
						return jarEditorInput;
					}
				}
			}
		} catch (JavaModelException e) {
			ExtensionsPlugin.getDefault().logError(e);
		} catch (CoreException e) {
			ExtensionsPlugin.getDefault().logError(e);
		} 
    	return null;
    }
}
