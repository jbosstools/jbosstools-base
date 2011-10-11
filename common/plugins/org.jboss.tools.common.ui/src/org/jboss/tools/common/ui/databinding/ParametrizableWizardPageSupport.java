/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.dialog.DialogPageSupport;
import org.eclipse.jface.wizard.WizardPage;

/**
 * @author André Dietisheim
 */
public class ParametrizableWizardPageSupport extends DialogPageSupport {

	private int nonValidatingSeverity;

	private ParametrizableWizardPageSupport(int nonValidatingSeverity, WizardPage wizardPage, DataBindingContext dbc) {
		super(wizardPage, dbc);
		this.nonValidatingSeverity = nonValidatingSeverity;
	}

	/**
	 * Creates a wizard page support that will not validate if the validation
	 * status is IStatus#ERROR or IStatus#CANCEL.
	 * 
	 * @param wizardPage
	 *            the wizardpage to apply this support to
	 * @param dbc
	 *            the databinding context to use
	 * @return the wizard page support that was created
	 */
	public static ParametrizableWizardPageSupport create(WizardPage wizardPage, DataBindingContext dbc) {
		return create(IStatus.ERROR | IStatus.CANCEL, wizardPage, dbc);
	}

	/**
	 * Creates a wizard page support that will not validate for the given status
	 * mask (severity).
	 * 
	 * @param nonValidatingSeverity the status severity mask that will not validate
	 * @param wizardPage
	 *            the wizardpage to apply this support to
	 * @param dbc
	 *            the databinding context to use
	 * @return the wizard page support that was created
	 */
	public static ParametrizableWizardPageSupport create(int nonValidatingSeverity, WizardPage wizardPage,
			DataBindingContext dbc) {
		return new ParametrizableWizardPageSupport(nonValidatingSeverity, wizardPage, dbc);
	}

	protected void handleStatusChanged() {
		super.handleStatusChanged();
		boolean pageComplete = true;
		if (currentStatusStale) {
			pageComplete = false;
		} else if (currentStatus != null) {
			pageComplete = !((nonValidatingSeverity | currentStatus.getSeverity()) == nonValidatingSeverity);
		}
		((WizardPage) getDialogPage()).setPageComplete(pageComplete);
	}
}
