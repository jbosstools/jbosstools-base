package org.jboss.tools.runtime.reddeer.condition;

import org.jboss.reddeer.common.condition.AbstractWaitCondition;
import org.jboss.reddeer.core.condition.JobIsRunning;
import org.jboss.reddeer.core.condition.ShellWithTextIsAvailable;
import org.jboss.reddeer.swt.impl.button.PushButton;
import org.jboss.reddeer.swt.impl.shell.DefaultShell;

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
