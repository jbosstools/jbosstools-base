package org.jboss.tools.runtime.reddeer.condition;

import org.eclipse.reddeer.common.condition.AbstractWaitCondition;
import org.eclipse.reddeer.common.wait.WaitWhile;
import org.eclipse.reddeer.workbench.core.condition.JobIsRunning;
import org.eclipse.reddeer.swt.api.Shell;
import org.eclipse.reddeer.swt.condition.ShellIsAvailable;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;

public class RuntimeIsDownloading extends AbstractWaitCondition{

	@Override
	public boolean test() {
		if(new ShellIsAvailable("Question").test()){
			Shell questionShell = new DefaultShell("Question");
			new PushButton(questionShell, "Yes To All").click();
			new WaitWhile(new ShellIsAvailable(questionShell));
		}
		return new JobIsRunning().test();
	}

	@Override
	public String description() {
		return "Runtime is downloading";
	}

}
