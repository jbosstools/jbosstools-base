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
package org.jboss.tools.common.model.ui.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.jboss.tools.common.meta.action.XAction;

/**
 * Converts action description of XModel into Eclipse action.
 * @author glory
 */
public class ActionX extends Action {
	XAction action;
	XModelObjectAction wrapper;
	
	public ActionX(XModelObjectAction a) {
		wrapper = a;
		this.action = a.action;
		String displayName = action.getDisplayName();

		String baseName = action.getBaseActionName();
		if("Copy".equals(baseName)) {
			setAccelerator(SWT.CONTROL | 'C');
			displayName += '\t' + "Ctrl + C"; 
		} else if("Cut".equals(baseName)) {
			setAccelerator(SWT.CONTROL | 'X');
			displayName += '\t' + "Ctrl + X"; 
		} else if("Delete".equals(baseName)) {
			setAccelerator(SWT.DEL);
			displayName += '\t' + "Delete"; 
		} else if("Paste".equals(baseName)) {
			setAccelerator(SWT.CONTROL | 'V');
			displayName += '\t' + "Ctrl + V"; 
		} else if("Open".equals(baseName)) {
			setAccelerator(SWT.F3);
			displayName += '\t' + "F3"; 
		}

		setText(displayName);

		try {
			String iconKey = action.getIconKey();
			Image image = action.getMetaModel().getIconList().getImage(iconKey);
			setImageDescriptor(new XImageDescriptor(image));
		} catch (Exception e) {
			Image image = action.getMetaModel().getIconList().getImage("action.empty");
			setImageDescriptor(new XImageDescriptor(image));
		}
		boolean enabled = (wrapper.targets == null) ? action.isEnabled(wrapper.object) : action.isEnabled(wrapper.object, wrapper.targets);
		setEnabled(enabled);
	}
	
	class XImageDescriptor extends ImageDescriptor {
		Image image;
		XImageDescriptor(Image image) {
			this.image = image;
		}

		public Image createImage() {
			return image;
		}
		public Image createImage(boolean returnMissingImageOnError, Device device) {
			return image;
		}

		@Override
		public ImageData getImageData() {
			return null;
		}
		
	}
	
	public void run() {
		wrapper.actionPerformed();
	}

}
