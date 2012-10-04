package org.jboss.tools.runtime.as.ui.bot.test;

import org.jboss.reddeer.junit.runner.RedDeerSuite;
import org.jboss.tools.ui.bot.ext.MacSpecifics;
import org.jboss.tools.ui.bot.ext.SWTJBTExt;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;

public class JBTSuite extends RedDeerSuite {

	public JBTSuite(Class<?> clazz, RunnerBuilder builder)
			throws InitializationError {
		super(clazz, builder);
	}

	@Override
	public void run(RunNotifier arg0) {
		MacSpecifics.setupToolkit();
		SWTJBTExt.manageBlockingWidows(false, false);
		MacSpecifics.setupJava();
		super.run(arg0);
	}
}
