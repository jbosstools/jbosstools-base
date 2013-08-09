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

import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class XModelObjectIcon {
    static Hashtable<String,ImageComponent> components = null;

    private static synchronized void load(XModelObject object) {
        if(components != null || object == null) return;
        components = new Hashtable<String,ImageComponent>();
        XModel model = object.getModel();
        XMapping mapping = model.getMetaData().getMapping("Icons"); //$NON-NLS-1$
        if(mapping == null) return;
        String[] keys = mapping.getKeys();
        for (int i = 0; i < keys.length; i++) {
            String v = mapping.getValue(keys[i]);
            components.put(keys[i], new ImageComponentWrapper(keys[i], v));
        }
    }

    private XModelObject object = null;

    public XModelObjectIcon(XModelObject object) {
        load(object);
        this.object = object;
    }

    public Image getIcon1(String[] types) {
        if(object == null) return null;
        String code = "" + getIconHash(types); //$NON-NLS-1$
        ImageRegistry registry = ModelPlugin.getDefault().getImageRegistry();
        Image ii = null;
        synchronized (registry) {
            ii = registry.get(code);
		}
        if(ii != null && !ii.isDisposed()) {
        	return ii;
        }
        Vector<Image> v = new Vector<Image>(3);
        for (int i = 0; i < types.length; i++) {
            ImageComponent component = components.get(types[i]);
            Image ic = (component == null) ? null : component.getImage(object);
            if(ic != null) {
            	v.addElement(ic);
            	return ic;
            }
        }
        return ii;
    }

	public int getIconHash() {
		return getIconHash(object.getModelEntity().getRenderer().getIconNames());
	}

	private int getIconHash(String[] types) {
		int res = 0;
		for (int i = 0; i < types.length; i++) {
			ImageComponent component = components.get(types[i]);
			if(component != null) res += component.getHash(object);
		}
		return res;
	}

	public Image getEclipseImage() {
		return getEclipseImage0(object.getModelEntity().getRenderer().getIconNames());
	}
    
	public Image getEclipseImage0(String[] types) {
		if(object == null) return null;
		String code = "" + getIconHash(types); //$NON-NLS-1$
		ImageRegistry registry = ModelPlugin.getDefault().getImageRegistry();
		Image iie = null;
		synchronized(registry) {
			iie = registry.get(code);
		}
		if(iie != null && !iie.isDisposed()) {
			return iie;
		}
		Image ii = getIcon1(types);
		if(ii != null) {
			synchronized(registry) {
				registry.remove(code);
				registry.put(code, ii);
			}
		}
		return ii;
	}

}

class ImageComponentWrapper implements ImageComponent {
	String key;
	String classname;
	ImageComponent imageComponent;
	
	ImageComponentWrapper(String key, String classname) {
		this.key = key;
		this.classname = classname;
	}

	public int getHash(XModelObject obj) {
		validate();
		if(imageComponent == null) return 0;
		return imageComponent.getHash(obj);
	}

	public Image getImage(XModelObject obj) {
		validate();
		if(imageComponent == null) return null;
		return imageComponent.getImage(obj);
	}
	
	void validate() {
        try {
            ImageComponent c = (ImageComponent)ModelFeatureFactory.getInstance().createFeatureInstance(classname);
            if(c == null) {
            	XModelObjectIcon.components.remove(key);
            } else {
            	XModelObjectIcon.components.put(key, c);
            }
            imageComponent = c;
        } catch (ClassCastException e) {
        	ModelPlugin.getPluginLog().logError(e);
        	XModelObjectIcon.components.remove(key);
        }
	}
	
}