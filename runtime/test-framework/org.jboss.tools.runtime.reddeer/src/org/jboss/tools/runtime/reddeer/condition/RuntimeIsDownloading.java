package org.jboss.tools.runtime.reddeer.condition;

import org.eclipse.reddeer.common.condition.AbstractWaitCondition;
import org.eclipse.reddeer.core.condition.JobIsRunning;
import org.eclipse.reddeer.core.condition.ShellWithTextIsAvailable;
import org.eclipse.reddeer.swt.impl.button.PushButton;
import org.eclipse.reddeer.swt.impl.shell.DefaultShell;

public class RuntimeIsDownloading extends AbstractWaitCondition{

	@Override
	public boolean test() {
		if(new ShellWithTextIsAvailable("Question").test()){
			new DefaultShell("Question");
			new PushButton("Yes To All").click();
		}
		return new JobIsRunning().test();
	}

	@Override
	public String description() {
		return "Runtime is downloading";
	}

}
