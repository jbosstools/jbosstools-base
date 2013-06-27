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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.util.Properties;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.views.palette.PaletteInsertHelper;

public class PlainTextDropCommand extends DefaultDropCommand {

	public void run(IProgressMonitor monitor) throws CoreException {
		String data = getDefaultModel().getDropData().getMimeData();
		if(data == null) return;
		Properties properties = new Properties();
		properties.put(PaletteInsertHelper.PROPERTY_START_TEXT, data);
		PaletteInsertHelper.getInstance().insertIntoEditor(
				getDefaultModel().getDropData().getSourceViewer(),
				properties
		);
	}

	public void initialize() {
	}

	public void execute() {
	}

	public void execute(DropData data) {
		getDefaultModel().setDropData(data);		
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		try {
			workspace.run(this,new NullProgressMonitor());
		} catch (CoreException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		getDefaultModel().setDropData(null);
	}
}