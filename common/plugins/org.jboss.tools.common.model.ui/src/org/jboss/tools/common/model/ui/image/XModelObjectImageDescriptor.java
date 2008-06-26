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
package org.jboss.tools.common.model.ui.image;

import java.util.Hashtable;

import org.eclipse.swt.graphics.*;
import org.eclipse.jface.resource.ImageDescriptor;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.icons.impl.XModelObjectIcon;

public class XModelObjectImageDescriptor extends ImageDescriptor {
	private static Hashtable<String,Image> imageCache = new Hashtable<String,Image>();
	private XModelObjectIcon xicon = null;

	public XModelObjectImageDescriptor(XModelObject object) {
		xicon = new XModelObjectIcon(object);
	}

	public ImageData getImageData() {
		return createImage().getImageData();
	}

	public Image createImage(boolean returnMissingImageOnError, Device device) {
		int code = xicon.getIconHash();
		if (code == 0) return null;
		String key = "" + code;
		Image img = (Image)imageCache.get(key);
		if (img != null) return img;
		img = xicon.getEclipseImage();
		if (img != null) imageCache.put(key, img);
		return img;
	}
}
