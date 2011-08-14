/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.actions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;
import org.jboss.tools.common.jdt.debug.ui.Messages;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.jboss.tools.common.jdt.debug.ui.preferences.RemoteDebug;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteDebugItems extends CompoundContributionItem {

	@Override
	protected IContributionItem[] getContributionItems() {
		List<IContributionItem> items = new ArrayList<IContributionItem>();
		List<RemoteDebug> remoteDebugs = RemoteDebugUIActivator.getDefault()
				.getValidRemoteDebugs();
		VmModel[] models = RemoteDebugUIActivator.getDefault().getDebugModels(new NullProgressMonitor());
		if (models != null) {
			for (VmModel model : models) {
				RemoteLaunchAction action = new RemoteLaunchAction(
						model.getPort());
				RemoteDebug remoteDebug = RemoteDebugUIActivator.findRemoteDebug(remoteDebugs,
						model.getPort());
				if (remoteDebug != null) {
					String id = RemoteDebugUIActivator.COMMAND_PREFIX
							+ remoteDebug.getId();
					action.setActionDefinitionId(id);
					if (remoteDebug.isShow()) {
						StringBuffer buffer = new StringBuffer();
						if (remoteDebug.getDescription() != null
								&& !remoteDebug.getDescription().isEmpty()) {
							if (buffer.length() > 0) {
								buffer.append(","); //$NON-NLS-1$
							}
							buffer.append(remoteDebug.getDescription());
						}
						String text = model.getMainClass();
						if (text != null
								&& !RemoteDebugActivator.UNKNOWN.equals(text)) {
							if (buffer.length() > 0) {
								buffer.append(",main="); //$NON-NLS-1$
							}
							buffer.append(text);
						}
						String pid = model.getPid();
						if (pid != null) {
							if (buffer.length() > 0) {
								buffer.append(",pid="); //$NON-NLS-1$
							}
							buffer.append(pid);
						}
						String port = model.getPort();
						if (port != null) {
							if (buffer.length() > 0) {
								buffer.append(",port="); //$NON-NLS-1$
							}
							buffer.append(port);
						}
						action.setText(buffer.toString());
					} else {
						action.setText(model.getDisplayName());
					}
				} else {
					action.setText(model.getDisplayName());
				}
				ActionContributionItem item = new ActionContributionItem(
						action);
				items.add(item);
			}

		}
		if (items.size() > 0) {
			items.add(new Separator());
		}
		if (!RemoteDebugUIActivator.getDefault().isDiscoverRemoteApplication()) {
			IAction action = new DiscoverRemoteApplicationAction();
			String id = RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION_ACTION_ID;
			action.setActionDefinitionId(id);
			action.setText(Messages.Discover_Remote_Applications);
			ActionContributionItem item = new ActionContributionItem(action);
			items.add(item);
		}
		IAction action = new LaunchDialogAction();
		String id = RemoteDebugUIActivator.CONFIGURE_ACTION_ID;
		action.setActionDefinitionId(id);
		action.setText(Messages.RemoteDebugItems_Configure);
		ActionContributionItem item = new ActionContributionItem(action);
		items.add(item);
		if (items.size() > 0) {
			return items.toArray(new IContributionItem[0]);
		}
		return null;
	}

}
