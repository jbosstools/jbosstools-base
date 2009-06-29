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

import java.awt.*;

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.IconUtil;
import org.jboss.tools.common.model.icons.*;
import org.jboss.tools.common.meta.impl.*;

public class ModifiersIcon implements ImageComponent {

    public static Image ABS, FIN, PBC, PLC, PTC, PVC, PBS, PLS, PTS, PVS;

    static {
        XIconList list = XModelMetaDataImpl.getInstance().getIconList();
        ABS = list.getImage("modifiers.abstract"); //$NON-NLS-1$
        FIN = list.getImage("modifiers.final"); //$NON-NLS-1$
        PBC = list.getImage("modifiers.public"); //$NON-NLS-1$
        PTC = list.getImage("modifiers.protected"); //$NON-NLS-1$
        PLC = list.getImage("modifiers.package_local"); //$NON-NLS-1$
        PVC = list.getImage("modifiers.private"); //$NON-NLS-1$
        PBS = list.getImage("modifiers.public_static"); //$NON-NLS-1$
        PTS = list.getImage("modifiers.protected_static"); //$NON-NLS-1$
        PLS = list.getImage("modifiers.package_local_static"); //$NON-NLS-1$
        PVS = list.getImage("modifiers.private_static"); //$NON-NLS-1$
    }

    static Image add(Image front, Image back) {
    	//TODO implement
//        if(front == null || back == null) return IconUtil.getEmptyIcon(new Point(16, 16));
//        Point p = new Point(back.getIconWidth(), front.getIconHeight());
//        return IconUtil.placeImages(p, back, new Point(0,0),
//                                       front, new Point(0,0), true);
    	
    	if(front != null) return front;
    	return back;
    }

    public static Image[] MOD = new Image[] {PBC, PLC, PTC, PVC};

    public static Image[] MOD_S = new Image[]
        {add(PBC, PBS), add(PLC, PLS), add(PTC, PTS), add(PVC, PVS)};

    public static Image[] MOD_A = new Image[4];
    public static Image[] MOD_F = new Image[4];
    public static Image[] MOD_SA = new Image[4];
    public static Image[] MOD_SF = new Image[4];

    static {
        for (int i = 0; i < 4; i++) {
            MOD_A[i] = add(ABS, MOD[i]);
            MOD_F[i] = add(FIN, MOD[i]);
            MOD_SA[i] = add(ABS, MOD_S[i]);
            MOD_SF[i] = add(FIN, MOD_S[i]);
        }
    }

    public ModifiersIcon() {}

    public int getHash(XModelObject obj) {
        if(!isShowingModifiers(obj)) return 0;
        String mod = obj.getAttributeValue("modifiers"); //$NON-NLS-1$
        return (mod == null) ? 0 : mod.hashCode();
    }

    public Image getImage(XModelObject obj) {
        if(!isShowingModifiers(obj)) return null;
        String mod = obj.getAttributeValue("modifiers"); //$NON-NLS-1$
        int access = (mod.indexOf("public") >= 0) ? 0 : //$NON-NLS-1$
                     (mod.indexOf("protected") >= 0) ? 2 : //$NON-NLS-1$
                     (mod.indexOf("private") >= 0) ? 3 : 1; //$NON-NLS-1$
        boolean isStatic = (mod.indexOf("static") >= 0); //$NON-NLS-1$
        int override = (mod.indexOf("final") >= 0) ? 1 : //$NON-NLS-1$
                       (mod.indexOf("abstract") >= 0) ? 2 : 0; //$NON-NLS-1$
        return getModifiersIcon(access, isStatic, override);
    }

    private boolean isShowingModifiers(XModelObject obj) {
        XModelObject o = obj.getModel().getRoot("Preferences"); //$NON-NLS-1$
        return (o == null) ? true : XModelObjectConstants.YES.equals(o.getAttributeValue("show modifiers")); //$NON-NLS-1$
    }

    private Image getModifiersIcon(int access, boolean isStatic, int override) {
        return (isStatic) ? getStaticIcon(access, override) : getNonStaticIcon(access, override);
    }

    private Image getStaticIcon(int i, int override) {
        return (override == 1) ? MOD_SF[i] :
               (override == 2) ? MOD_SA[i] : MOD_S[i];
    }

    private Image getNonStaticIcon(int i, int override) {
        return (override == 1) ? MOD_F[i] :
               (override == 2) ? MOD_A[i] : MOD[i];
    }

}
