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

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.impl.*;

public class ReferencedIcons implements ImageComponent {
    private XStudioIcons studioicons = new XStudioIcons();

    public ReferencedIcons() {}

    public int getHash(XModelObject obj) {
        String x = obj.getAttributeValue("icon");
        if (x == null || x.trim().length() == 0) return
          ((XEntityRendererImpl)obj.getModelEntity().getRenderer()).getIconInfo("imageref").hashCode();
        XModelObject r = obj.getModel().getByPath(x);
        return (r == null) ? "defaultimage".hashCode() : 718 + x.hashCode() + studioicons.getHash(r);
    }

    public Image getImage(XModelObject obj) {
        String v = obj.getAttributeValue("icon");
        if(v == null || v.trim().length() == 0) {
            String s = ((XEntityRendererImpl)obj.getModelEntity().getRenderer()).getIconInfo("imageref");
            return obj.getModelEntity().getMetaModel().getIconList().getImage(s);
        }
        XModelObject ic = (v == null || v.trim().length() == 0) ? null : obj.getModel().getByPath(v);
        Image res = (ic == null || ic == obj) ? null : new XModelObjectIcon(ic).getEclipseImage();
        if(res != null) return res;
        return obj.getModelEntity().getMetaModel().getIconList().getImage("default.unknown");
    }

}

