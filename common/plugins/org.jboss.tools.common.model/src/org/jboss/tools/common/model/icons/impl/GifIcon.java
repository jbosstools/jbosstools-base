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
package org.jboss.tools.common.model.icons.impl;

import javax.swing.ImageIcon;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class GifIcon implements ImageComponent {

    public GifIcon() {}

    public int getHash(XModelObject obj) {
        String p = obj.getPath();
        if(p == null) p = obj.getAttributeValue("name");
        return (p == null) ? 0 : p.hashCode();
    }  

    public ImageIcon getIcon(XModelObject obj) {
        try {
            String p = XModelObjectLoaderUtil.getResourcePath(obj);
            if(p == null) return null;
            p = p.substring(1);
            java.net.URL url = obj.getModel().getModelClassLoader().getResource(p);
            ImageIcon icon = new ImageIcon(url);
            return (icon.getIconWidth() > 20) ? null : icon;
        } catch (Exception e) {
            return null;
        }
    }

    public Image getImage(XModelObject obj) {
    	return null;
    }

}
