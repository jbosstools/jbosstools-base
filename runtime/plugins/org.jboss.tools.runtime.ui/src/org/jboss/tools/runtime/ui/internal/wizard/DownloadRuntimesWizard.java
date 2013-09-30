/*******************************************************************************
 * Copyright (c) 2000, 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.util.List;

import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.ui.wizard.DownloadRuntimesTaskWizard;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimesWizard extends DownloadRuntimesTaskWizard {

	public DownloadRuntimesWizard() {
		super();
	}
	
	public DownloadRuntimesWizard(Shell shell) {
		super();
	}

	public DownloadRuntimesWizard(Shell shell, IDownloadRuntimeFilter filter) {
		super(filter);
	}
	
	public DownloadRuntimesWizard(Shell shell, List<DownloadRuntime> runtimes) {
		super(runtimes);
	}
}
