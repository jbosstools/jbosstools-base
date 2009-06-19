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

import org.eclipse.jdt.internal.core.JarEntryFile;
import org.eclipse.jdt.internal.ui.javaeditor.JarEntryEditorInput;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
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
		} else { 
			super.doHyperlink(region);
		}
	}
	
}
