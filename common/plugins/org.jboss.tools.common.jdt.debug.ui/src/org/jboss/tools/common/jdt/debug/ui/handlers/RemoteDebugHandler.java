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
package org.jboss.tools.common.jdt.debug.ui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.jboss.tools.common.jdt.debug.ui.actions.RemoteLaunchAction;
import org.jboss.tools.common.jdt.debug.ui.preferences.RemoteDebug;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteDebugHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		Command command = event.getCommand();
		String id = command.getId();
		String[] segments = id.split("\\.");
		String keyStr = segments[segments.length-1];
		int key = new Integer(keyStr);
		RemoteDebug remoteDebug = RemoteDebugUIActivator.getDefault().getRemoteDebugs()[key];
		if (remoteDebug.isValid()) {
			final Action action = new RemoteLaunchAction(remoteDebug.getPort());
			Display.getDefault().syncExec(new Runnable() {
				
				@Override
				public void run() {
					action.run();
				}
			});
			
		}
		return null;
	}

}
