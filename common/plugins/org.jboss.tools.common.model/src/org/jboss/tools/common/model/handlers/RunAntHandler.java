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

import java.text.MessageFormat;
import java.util.Properties;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.IObjectActionDelegate;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.extension.ExtensionPointUtil;

/**
 * @author victor
 */
public class RunAntHandler extends AbstractHandler {
	public boolean isEnabled(XModelObject object) {
		return (object != null && object.isActive());
	}

	public void executeHandler(XModelObject object, Properties p) throws XModelException {
		ILaunchShortcut sc = findLaunchShortcut("org.eclipse.ant.ui.antShortcutWithDialog"); //$NON-NLS-1$
		IFile file = (IFile)object.getAdapter(IFile.class);
		sc.launch(new StructuredSelection(file), ILaunchManager.RUN_MODE);
	}
	
	static IObjectActionDelegate findPopupMenusObjectAction(String pluginId, String actionId) {
		Platform.getBundle(pluginId);
		try	{
			return (IObjectActionDelegate)ExtensionPointUtil.findClassByElementId("org.eclipse.ui.popupMenus", actionId); //$NON-NLS-1$
		} catch (IllegalArgumentException ex) {
			//ignore
		} catch (CoreException ex) {
			//ignore
		} catch (ClassCastException ex) {
			ModelPlugin.getPluginLog().logError(ex);
		}
		return null;
	}
	
	static ILaunchShortcut findLaunchShortcut(String shortcutId) throws XModelException {
		String pointId = "org.eclipse.debug.ui.launchShortcuts"; //$NON-NLS-1$
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(pointId);
		if(point == null) throw new XModelException(MessageFormat.format("Cannot find extension point {0}", pointId));
		IConfigurationElement[] es = point.getConfigurationElements();
		for (int i = 0; i < es.length; i++) {
			if(!shortcutId.equals(es[i].getAttribute("id"))) continue; //$NON-NLS-1$
			try {
				return (ILaunchShortcut)es[i].createExecutableExtension("class"); //$NON-NLS-1$
			} catch (CoreException e) {
				throw new XModelException(e);
			}
		}
		throw new XModelException(MessageFormat.format("Cannot find launch shortcut {0}", shortcutId));
	}
}
