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

import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

public interface IDropCommand extends IWorkspaceRunnable {
	/**
	 * Can be used by tests to set as system property 
	 *            System.setProperty(TEST_FLAG, "true");
	 * Code to be tested can use this flag to run code 
	 * in the same thread and open dialogs non-modally.
	 */
	public String TEST_FLAG = "org.jboss.tools.common.test";

	/**
	 * 
	 */
	public void run(IProgressMonitor monitor) throws CoreException;

	/**
	 * 
	 * @return
	 */
	public IDropWizardModel getDefaultModel();

	/**
	 *
	 */
	public void initialize();

	/**
	 *
	 */
	public void execute();

	/**
	 * 
	 * @param data
	 */
	public void execute(DropData data);

	public void setTagProposalFactory(ITagProposalFactory tagProposalFactory);
	public ITagProposalFactory getTagProposalFactory();
}