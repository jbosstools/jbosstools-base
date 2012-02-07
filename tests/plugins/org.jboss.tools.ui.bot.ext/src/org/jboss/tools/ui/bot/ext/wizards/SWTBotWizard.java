/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.wizards;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.condition.NonSystemJobRunsCondition;
import org.jboss.tools.ui.bot.ext.condition.ProgressInformationShellIsActiveCondition;
import org.jboss.tools.ui.bot.ext.condition.ShellIsActiveCondition;
import org.jboss.tools.ui.bot.ext.condition.TaskDuration;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

public class SWTBotWizard extends SWTBotShell {	

	public SWTBotWizard() {
		this(SWTTestExt.bot.activeShell().widget);
	}

	public SWTBotWizard(Shell shell) {
		super(shell);
		assert getText().contains("New ");
	}

	public SWTBotWizard selectTemplate(String... item) {
		SWTTestExt.open.selectTreeNode(this.bot(),item);
		return this;
	}

	public SWTBotWizard back() {
		clickButton(IDELabel.Button.BACK);
		return this;
	}

	public SWTBotWizard next() {
		clickButton(IDELabel.Button.NEXT);
		return this;
	}

	public SWTBotWizard nextWithWait() {
		SWTBotShell activeShell = getActiveShell();
		next();
		bot().waitUntil(new ShellIsActiveCondition(activeShell));
		return this;
	}

	public void cancel() {
		clickButton(IDELabel.Button.CANCEL);
	}

	public void finish() {
		clickButton(IDELabel.Button.FINISH);
	}

	public void finishWithWait() {
		SWTBotShell activeShell = getActiveShell();
		finish();
		bot().waitWhile(new ShellIsActiveCondition(activeShell), TaskDuration.LONG.getTimeout());
		bot().waitWhile(new ProgressInformationShellIsActiveCondition(), TaskDuration.LONG.getTimeout());
		bot().waitWhile(new NonSystemJobRunsCondition(), TaskDuration.LONG.getTimeout());
	}

	protected void clickButton(String text) {
		bot().button(text).click();
		bot().sleep(500);
	}

	protected void setText(String label, String text) {
		SWTBotText t = bot().textWithLabel(label);
		t.setFocus();
		t.setText(text);
	}

	private SWTBotShell getActiveShell(){
		for (SWTBotShell shell : bot().shells()){
			if (shell.isActive()){
				return shell;
			}
		}

		throw new IllegalStateException("No active shell found");
	}
}
