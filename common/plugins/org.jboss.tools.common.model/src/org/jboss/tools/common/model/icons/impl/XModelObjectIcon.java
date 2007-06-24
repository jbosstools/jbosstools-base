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

import java.util.*;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class XModelObjectIcon {
	private static Hashtable<String,Image> cacheEclipse = new Hashtable<String,Image>();
    private static Hashtable<String,ImageComponent> components = null;

    private static synchronized void load(XModelObject object) {
        if(components != null || object == null) return;
        components = new Hashtable<String,ImageComponent>();
        XModel model = object.getModel();
        XMapping mapping = model.getMetaData().getMapping("Icons");
        if(mapping == null) return;
        String[] keys = mapping.getKeys();
        for (int i = 0; i < keys.length; i++) {
            String v = mapping.getValue(keys[i]);
            try {
                ImageComponent c = (ImageComponent)ModelFeatureFactory.getInstance().createFeatureInstance(v);
                components.put(keys[i], c);
            } catch (Exception e) {
				///XStudioPlugin.getDefault().getLog().log(new Status(Status.ERROR, XStudioPlugin.PLUGIN_ID, Status.OK, "Cannot load icon " + keys[i] + " " + v,e));
            }
        }
    }

    private XModelObject object = null;

    public XModelObjectIcon(XModelObject object) {
        load(object);
        this.object = object;
    }

    public Image getIcon1(String[] types) {
        if(object == null) return null;
        String code = "" + getIconHash(types);
        Image ii = cacheEclipse.get(code);
        if(ii != null) return ii;
        Vector<Image> v = new Vector<Image>(3);
        for (int i = 0; i < types.length; i++) {
            ImageComponent component = components.get(types[i]);
            Image ic = (component == null) ? null : component.getImage(object);
            if(ic != null) {
            	v.addElement(ic);
            	return ic;
            }
        }
/*
 TODO add icons
        if(v.size() == 0) v.addElement(IconUtil.getEmptyIcon(new Point(16, 16)));
        ImageIcon[] icons = v.toArray(new ImageIcon[0]);
        ii = IconUtil.getRowImage(icons);
        if(ii != null) cache.put(code, ii);
*/
        return ii;
    }

	public int getIconHash() {
		try {
			return getIconHash(object.getModelEntity().getRenderer().getIconNames());
		} catch (Exception e) {
			return 0;
		}
	}

	private int getIconHash(String[] types) {
		int res = 0;
		for (int i = 0; i < types.length; i++) {
			ImageComponent component = (ImageComponent)components.get(types[i]);
			if(component != null) res += component.getHash(object);
		}
		return res;
	}

	public Image getEclipseImage() {
		try {
			return getEclipseImage0(object.getModelEntity().getRenderer().getIconNames());
		} catch (Exception e) {
			return null;
		}
	}
    
	public Image getEclipseImage0(String[] types) {
		if(object == null) return null;
		String code = "" + getIconHash(types);
		Image iie = cacheEclipse.get(code);
		if(iie != null) return iie;
		Image ii = getIcon1(types);
		if(ii != null) cacheEclipse.put(code, ii);
		return ii;
	}

}
