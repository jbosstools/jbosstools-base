/*******************************************************************************
 * Copyright (c) 2003, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - Initial API and implementation
 *******************************************************************************/
package org.jboss.tools.foundation.ui.xpl.taskwizard;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.jboss.tools.foundation.ui.internal.Trace;

/**
 * A task wizard page.
 * @Since 1.1
 */
public class TaskWizardPage extends WizardPage implements IWizardHandle {
	protected WizardFragment fragment;

	protected boolean isCreated = false;

	public TaskWizardPage(WizardFragment fragment) {
		super(fragment.toString());
		this.fragment = fragment;
	}

	public void createControl(Composite parentComp) {
		Composite comp = null;
		try {
			fragment.setPage(this);
			comp = fragment.createComposite(parentComp, this);
		} catch (Exception e) {
			Trace.trace(Trace.STRING_WARNING, "Could not create wizard page composite", e);//$NON-NLS-1$
		}
		if (comp == null) {
			comp = new Composite(parentComp, SWT.NONE);
			comp.setLayout(new FillLayout(SWT.VERTICAL));
			Label label = new Label(comp, SWT.NONE);
			label.setText("Internal error");//$NON-NLS-1$
		}
		GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		data.widthHint = convertHorizontalDLUsToPixels(150);
		comp.setLayoutData(data);
		setControl(comp);
		
		isCreated = true;
		update();
	}

	public boolean isPageComplete() {
		try {
			if (!fragment.isComplete())
				return false;
		} catch (Exception e) {
			Trace.trace(Trace.STRING_WARNING, "Exception caught checking if page is complete", e);//$NON-NLS-1$
			return false;
		}
		return true;
	}

	public boolean canFlipToNextPage() {
		if (getNextPage() == null)
			return false;
		try {
			if (!fragment.isComplete())
				return false;
		} catch (Exception e) {
			Trace.trace(Trace.STRING_WARNING, "Exception caught checking if page is complete", e);//$NON-NLS-1$
			return false;
		}
		return true;
	}

	public void setVisible(boolean visible) {
		if (visible) {
			TaskWizard wizard = (TaskWizard) getWizard();
			wizard.switchWizardFragment(fragment);
			
			if (getContainer().getCurrentPage() != null)
				getContainer().updateButtons();
		}
		super.setVisible(visible);
	}

	public void setMessage(String message, int type) {
		if (type == IMessageProvider.ERROR && "".equals(message)) {//$NON-NLS-1$
			message = null;
		}
		super.setMessage(message, type);
		WizardFragment frag = ((TaskWizard) getWizard()).getCurrentWizardFragment();
		if (!fragment.equals(frag))
			return;
		getContainer().updateButtons();
	}

	public void run(boolean fork, boolean cancelable, IRunnableWithProgress runnable) throws InterruptedException, InvocationTargetException {
		getWizard().getContainer().run(fork, cancelable, runnable);
	}

	public void update() {
		if (!isCreated)
			return;
		
		final IWizardContainer container = getContainer();
		getShell().getDisplay().syncExec(new Runnable() {
			public void run() {
				fragment.updateChildFragments();
				((TaskWizard) getWizard()).updatePages();
				
				if (container.getCurrentPage() != null)
					container.updateButtons();
			}
		});
	}
	
	/**
	 * If you must perform a long-running action when 
	 * next is pressed, return true. Otherwise false
	 * @return
	 */
	protected boolean hasActionOnNextPressed() {
		return fragment.hasActionOnNextPressed();
	}
	
	/**
	 * Perform your long-running action. 
	 * Return whether the page should be changed,
	 * or the action has failed. 
	 * 
	 * @return
	 */
	protected boolean performNextPressedAction() {
		return fragment.performNextPressedAction();
	}
}
