package org.jboss.tools.runtime.reddeer.wizard;

import org.jboss.reddeer.common.logging.Logger;
import org.jboss.reddeer.common.wait.TimePeriod;
import org.jboss.reddeer.common.wait.WaitUntil;
import org.jboss.reddeer.common.wait.WaitWhile;
import org.jboss.reddeer.core.condition.JobIsRunning;
import org.jboss.reddeer.jface.wizard.WizardDialog;
import org.jboss.reddeer.swt.api.Button;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.shell.DefaultShell;
import org.jboss.tools.runtime.reddeer.condition.RuntimeIsDownloading;

public class DownloadRuntimesTaskWizard extends WizardDialog{

	protected final static Logger log = Logger.getLogger(DownloadRuntimesTaskWizard.class);
	
	public void eapDialog(){
	}
	
	public void asDialog(){
	}
	
	@Override
	public void finish() {
		log.info("Finish wizard");
		new DefaultShell();
		Button button = new PushButton("Finish");
		button.click();
		new WaitUntil(new JobIsRunning(), TimePeriod.LONG, false);
		new WaitWhile(new RuntimeIsDownloading(), TimePeriod.getCustom(1200));
		new DefaultShell("New Project Example");
	}

}
