/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.model.impl;

import java.util.HashMap;
import java.util.Map;

import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectPresentation;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

/**
 * @author Viacheslav Kabanovich
 */
public class AnyElementPresentationManager implements XModelObjectPresentation {
	static AnyElementPresentationManager instance = new AnyElementPresentationManager();
	
	/**
	 * tag name to class name
	 */
	Map<String, String> tags = new HashMap<String, String>();
	
	/**
	 * class name to instance
	 */
	Map<String,XModelObjectPresentation> instances = new HashMap<String, XModelObjectPresentation>();
	
	public AnyElementPresentationManager() {
		init();
	}

	public String getValue(XModelObject object) {
		String tag = object.get("tag"); //$NON-NLS-1$
		if(tag == null) return null;
		String classname = tags.get(tag);
		if(classname == null) return null;
		XModelObjectPresentation p = instances.get(classname);
		if(p == null) {
			try {
				p = (XModelObjectPresentation)ModelFeatureFactory.getInstance().createFeatureInstance(classname);
			} catch (ClassCastException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
			if(p == null) {
				tags.remove(tag);
			} else {
				instances.put(classname, p);
			}
		}
		return p == null ? null : p.getValue(object);
	}

	private void init() {
		XMapping m = XModelMetaDataImpl.getInstance().getMapping("AnyElementPresentation"); //$NON-NLS-1$
		if(m == null) return;
		String[] ks = m.getKeys();
		for (int i = 0; i < ks.length; i++) {
			String cn = m.getValue(ks[i]);
			String[] qs = ks[i].split(","); //$NON-NLS-1$
			for (int j = 0; j < qs.length; j++) {
				tags.put(qs[j], cn);
			}
		}
	}
}
