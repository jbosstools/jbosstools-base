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
package org.jboss.tools.common.model.ui.navigator;

import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class NavigatorLabelProvider extends LabelProvider implements IColorProvider {
	
	public String getText(Object element) {
		String result = "";		
		if (element != null) {
			if (element instanceof XModelObject) {
				XModelObject o = (XModelObject)element; 
				result = o.getPresentationString();
				result = applyModification(o, result);
			} else
				result = element.toString();
		}			
		return (result == null) ? "" : result; 
	}
	
	protected String applyModification(XModelObject o, String text) {
		if(o.getFileType() == XFileObject.FILE && o.isModified()) text += "*";
		return text;
	}
	
	public Image getImage(Object element) {
		Image result = null;		
		if (element != null && element instanceof XModelObject)
			result = EclipseResourceUtil.getImage((XModelObject)element);			
		return result; 
	}

	public Color getForeground(Object element) {
		if (element instanceof XModelObject) {
			XModelObject o = (XModelObject)element;
			if(isLink(o)) return Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
			if(hasErrors(o)) return Display.getDefault().getSystemColor(SWT.COLOR_RED);
			if(hasParentFileErrors(o) /*&& !o.isObjectEditable()*/) return Display.getDefault().getSystemColor(SWT.COLOR_GRAY);
		}
		return null;
	}

	public Color getBackground(Object element) {
		return null;
	}

	private boolean isLink(XModelObject obj) {
		return "true".equals(obj.get("overlapped"));
	}

	private boolean hasErrors(XModelObject o) {
		return o != null && "yes".equals(o.get("_hasErrors_"));
	}
	
	private boolean hasParentFileErrors(XModelObject o) {
		if(o.getFileType() != XModelObject.NONE) return false;
		XModelObject p = o.getParent();
		while(p != null && p.getFileType() != XModelObject.FILE) p = p.getParent();
		return hasErrors(p);  
	}
}
