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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.*;

/**
 * 
 * @author eskimo
  */
public class LoadBundleBaseNameAttributeValueLoader implements IAttributeValueLoader {
	static String ATTR_BASENAME = "basename"; //$NON-NLS-1$
	static String ATTR_VAR = "var"; //$NON-NLS-1$

	public void fillTagAttributes(IDropWizardModel model) {
		String baseName = ""; //$NON-NLS-1$
		String fileName = ""; //$NON-NLS-1$

		IFile file = DropUtils.getResourceForMimeData(model.getDropData());
		IJavaElement javaElement = null;
		if(file == null || !file.exists() || file.getParent() == null) {
			String s = model.getDropData().getMimeData();
			if(s == null) return;
			s = s.replace('\\','/');
			int l = s.lastIndexOf('.');
			if(l >= 0) s = s.substring(0, l);
			int k = s.indexOf("/JavaSource/"); //$NON-NLS-1$
			if(k >= 0) {
				baseName = s.substring(k + "/JavaSource/".length()); //$NON-NLS-1$
			} else {
				baseName = s.substring(s.lastIndexOf('/') + 1);
			}
			baseName = baseName.replace('/', '.');
			fileName = baseName.substring(baseName.lastIndexOf('.') + 1);
		} else {
			XModelObject o = EclipseResourceUtil.getObjectByResource(file);
			if(o != null) {
				String path = XModelObjectLoaderUtil.getResourcePath(o);
				if(path.startsWith("/")) path = path.substring(1); //$NON-NLS-1$
				int i = path.lastIndexOf('.');
				if(i >= 0) path = path.substring(0,i);
				baseName = path.replace('/', '.');
				fileName = baseName.substring(baseName.lastIndexOf('.') + 1);
			} else {
				//incorrect
				javaElement = JavaCore.create((IFolder)file.getParent());
				fileName = file.getName();
				int i = fileName.lastIndexOf('.');
				if(i >= 0) fileName = fileName.substring(0,i);
				if(javaElement != null) {
					baseName = javaElement.getElementName()+ "." + fileName; //$NON-NLS-1$
				} else {
					baseName = fileName;
				}
			}
		}

		// TODO Eskimo - Think about how reject drop id properties file is not under java source
		model.setAttributeValue(ATTR_BASENAME, baseName);
		model.setAttributeValue(ATTR_VAR, fileName.toLowerCase());
		if(model instanceof DefaultDropWizardModel) {
			((DefaultDropWizardModel)model).setPreferable(ATTR_BASENAME);
			((DefaultDropWizardModel)model).setPreferable(ATTR_VAR);
		}
	}
	
}
