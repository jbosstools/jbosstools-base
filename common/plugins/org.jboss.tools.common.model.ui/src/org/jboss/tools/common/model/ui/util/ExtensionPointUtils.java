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
package org.jboss.tools.common.model.ui.util;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IObjectActionDelegate;
import org.jboss.tools.common.model.util.extension.ExtensionPointUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ExtensionPointUtils {
	
	public static INewWizard findNewWizardsItem(String pluginId, String wizardId) {
		Platform.getBundle(pluginId);
		try	{
			return (INewWizard)ExtensionPointUtil.findClassByElementId("org.eclipse.ui.newWizards", wizardId);
		} catch (Exception ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		return null;
	}
	
	public static IImportWizard findImportWizardsItem(String pluginId, String wizardId) {
		Platform.getBundle(pluginId);
		try	{
			return (IImportWizard)ExtensionPointUtil.findClassByElementId("org.eclipse.ui.importWizards", wizardId);
		} catch (Exception ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		return null;
	}

	
	public static IObjectActionDelegate findPopupMenusObjectAction(String pluginId, String actionId) {
		Platform.getBundle(pluginId);
		try	{
			return (IObjectActionDelegate)ExtensionPointUtil.findClassByElementId("org.eclipse.ui.popupMenus", actionId);
		} catch (Exception ex) {
			ModelUIPlugin.getPluginLog().logError(ex);
		}
		return null;
	}
	
}
