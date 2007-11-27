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
package org.jboss.tools.common.model.handlers;

import java.util.Properties;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.IObjectActionDelegate;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.extension.ExtensionPointUtil;

/**
 * @author victor
 */
public class RunAntHandler extends AbstractHandler {
	public boolean isEnabled(XModelObject object) {
		return (object != null && object.isActive());
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		ILaunchShortcut sc = findLaunchShortcut("org.eclipse.ant.ui.antShortcutWithDialog");
		IFile file = (IFile)object.getAdapter(IFile.class);
		sc.launch(new StructuredSelection(file), ILaunchManager.RUN_MODE);
	}
	
	static IObjectActionDelegate findPopupMenusObjectAction(String pluginId, String actionId) {
		Platform.getBundle(pluginId);
		try	{
			return (IObjectActionDelegate)ExtensionPointUtil.findClassByElementId("org.eclipse.ui.popupMenus", actionId);
		} catch (Exception ex) {
			///XStudioPlugin.log(ex);
		}
		return null;
	}
	
	static ILaunchShortcut findLaunchShortcut(String shortcutId) throws Exception {
		String pointId = "org.eclipse.debug.ui.launchShortcuts";
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(pointId);
		if(point == null) throw new Exception("Cannot find extension point " + pointId);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (int i = 0; i < es.length; i++) {
			if(!shortcutId.equals(es[i].getAttribute("id"))) continue;
			return (ILaunchShortcut)es[i].createExecutableExtension("class");
		}
		throw new Exception("Cannot find launch shortcut " + shortcutId);
	}
}
