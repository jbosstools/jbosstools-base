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
package org.jboss.tools.common.model.ui.attribute.adapter;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class DefaultXModelObjectLabelProvider extends LabelProvider {

	public DefaultXModelObjectLabelProvider() {
		super();
	}

	public String getText(Object element) {
		String result = "";
		if (element != null) {
			if (element instanceof XModelObject) {
				XModelObject modelObject = (XModelObject)element; 
				result = modelObject.getPresentationString();
				if(modelObject.getFileType() == XFileObject.FILE && modelObject.isModified()) result += "*";
			} else {
				result = element.toString();
			}
		}
		return result; 
	}
	
	public Image getImage(Object element) {
		if (element != null && element instanceof XModelObject) {
			return EclipseResourceUtil.getImage((XModelObject)element);
		}
		return null; 
	}
	
}
