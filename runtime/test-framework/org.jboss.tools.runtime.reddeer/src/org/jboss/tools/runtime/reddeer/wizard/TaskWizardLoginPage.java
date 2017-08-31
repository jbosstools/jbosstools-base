package org.jboss.tools.runtime.reddeer.wizard;

import org.eclipse.reddeer.common.exception.RedDeerException;
import org.eclipse.reddeer.common.wait.WaitWhile;
import org.eclipse.reddeer.core.reference.ReferencedComposite;
import org.eclipse.reddeer.jface.wizard.WizardPage;
import org.eclipse.reddeer.swt.api.Shell;
import org.eclipse.reddeer.swt.condition.ShellIsAvailable;
import org.eclipse.reddeer.swt.impl.button.CancelButton;
import org.eclipse.reddeer.swt.impl.button.OkButton;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.combo.LabeledCombo;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;
import org.eclipse.reddeer.swt.impl.text.LabeledText;

public class TaskWizardLoginPage extends WizardPage {

	public TaskWizardLoginPage(ReferencedComposite referencedComposite) {
		super(referencedComposite);
	}

	public void setUsername(String username) {
		new LabeledCombo(referencedComposite, "Username: ").setSelection(username);
	}

	public String getDomain() {
		return new LabeledCombo(referencedComposite, "Domain: ").getText();
	}

	public void addCredentials(String username, String password) {
		new PushButton(referencedComposite, "Add...").click();
		Shell credentialsShell = new DefaultShell("Add a Credential");
		new LabeledText(credentialsShell, "Username: ").setText(username);
		new LabeledText(credentialsShell, "Password: ").setText(password);
		new OkButton(credentialsShell).click();
		new WaitWhile(new ShellIsAvailable(credentialsShell));

		// Do not store this to secure storage
		handleSecureStorage();

	}

	private void handleSecureStorage() {
		cancelShellIfPresent("Secure Storage Password");
		cancelShellIfPresent("Secure Storage");
	}

	private void cancelShellIfPresent(String shellTitle) {
		try {
			Shell s = new DefaultShell(shellTitle);
			new CancelButton(s).click();
			new WaitWhile(new ShellIsAvailable(s));
		} catch (RedDeerException e) {
			// do nothing
		}
	}

	public boolean containsUsername(String username) {
		return new LabeledCombo(referencedComposite, "Username: ").getItems().contains(username);
	}

}
