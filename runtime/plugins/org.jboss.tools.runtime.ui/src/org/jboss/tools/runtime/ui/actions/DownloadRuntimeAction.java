/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.actions;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.dialogs.DownloadRuntimeDialog;

/** 
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimeAction extends Action {

	private String runtimeId;
	
	public DownloadRuntimeAction(String runtimeId) {
		super();
		setRuntimeId(runtimeId);
	}

	private void setRuntimeId(String runtimeId) {
		Assert.isNotNull(runtimeId);
		this.runtimeId = runtimeId;
	}

	public DownloadRuntimeAction(String text, ImageDescriptor image, String runtimeId) {
		super(text, image);
		setRuntimeId(runtimeId);
	}

	public DownloadRuntimeAction(String text, int style, String runtimeId) {
		super(text, style);
		setRuntimeId(runtimeId);
	}

	public DownloadRuntimeAction(String text, String runtimeId) {
		super(text);
		setRuntimeId(runtimeId);
	}

	@Override
	public void run() {
		Assert.isNotNull(runtimeId);
		// FIXME
		DownloadRuntime runtime = RuntimeCoreActivator.getDefault().getDownloadRuntimes().get(runtimeId);
		DownloadRuntimeDialog dialog = new DownloadRuntimeDialog(getShell(), runtime);
		dialog.open();
	}

	private Shell getShell() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		
	}

}
