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
package org.jboss.tools.common.meta.action.impl;

import org.eclipse.core.runtime.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.meta.*;

public abstract class CompoundAdoptManager implements XAdoptManager {

	public CompoundAdoptManager() {}

	protected synchronized final XAdoptManager[] loadManagers(String extensionPoint) {
		IExtensionPoint p = Platform.getExtensionRegistry().getExtensionPoint(extensionPoint);
		if(p == null) {
			ModelPlugin.getDefault().getLog().log(new Status(Status.ERROR, ModelPlugin.PLUGIN_ID, Status.OK, "Model warning: Cannot load extension point " + extensionPoint + ".", null));
			return new XAdoptManager[0];
		}
		IConfigurationElement[] es = p.getConfigurationElements();
		XAdoptManager[] array = new XAdoptManager[es.length];
		int length = 0;
		for (int i = 0; i < es.length; i++) {
			String cls = es[i].getAttribute("class");
			if(cls == null || cls.length() == 0) continue;
			try {
				XAdoptManager m = (XAdoptManager)es[i].createExecutableExtension("class");
				if(m != null) {
					array[length] = m;
					length++;
				}
			} catch (Exception e) {
				ModelPlugin.getDefault().getLog().log(new Status(Status.ERROR, ModelPlugin.PLUGIN_ID, Status.OK, "Model warning: Cannot load class " + cls + ".", e));
			}
		}
		if(length < array.length) {
			XAdoptManager[] array2 = new XAdoptManager[length];
			System.arraycopy(array, 0, array2, 0, length);
			return array2;
		}
		return array;
	}

	public abstract XAdoptManager[] getManagers();

	public boolean isAdoptable(XModelObject target, XModelObject object) {
		XAdoptManager[] ms = getManagers();
		for (int i = 0; i < ms.length; i++)
			if(ms[i].isAdoptable(target, object)) return true;
		return false;
	}

	public void adopt(XModelObject target, XModelObject object, java.util.Properties p) {
		XAdoptManager[] ms = getManagers();
		for (int i = 0; i < ms.length; i++) {
			if(ms[i].isAdoptable(target, object)) {
				ms[i].adopt(target, object, p);
				return;
			}
		}
	}

}
