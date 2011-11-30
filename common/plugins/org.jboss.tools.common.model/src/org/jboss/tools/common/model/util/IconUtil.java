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
package org.jboss.tools.common.model.util;

import org.eclipse.swt.graphics.Image;

public class IconUtil {
	public static final int PALETTE_GROUP_IMAGE_TYPE = 1; 
	public static final int PALETTE_ELEMENT_AS_GROUP_IMAGE_TYPE = 2; 
	public static final int PALETTE_ELEMENT_IMAGE_TYPE = 3; 
	public static final int PALETTE_IMAGE_WIDTH = 50; 

	public IconUtil() {}

	public static Image getEclipseImage(String imageName) {
		return ModelImages.getImage(imageName);
	}
}

