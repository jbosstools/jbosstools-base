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

import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ExceptionHandler extends org.eclipse.jdt.internal.ui.util.ExceptionHandler {

	private static ExceptionHandler instance = new ExceptionHandler();
	
	public static void handle(CoreException e, String title, String message) {
		Shell parent = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		instance.perform(e, parent, title, message);
	}
	
	public static void handle(InvocationTargetException e, String title, String message) {
		Shell parent = ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		instance.perform(e, parent, title, message);
	}
	
	protected void perform(CoreException e, Shell shell, String title, String message) {
		if (e.getStatus() != null) {
			ModelUIPlugin.getPluginLog().logError(e);
			//message ignored!
		} else {
			super.perform(e, shell, title, message);
		}
	}

}
