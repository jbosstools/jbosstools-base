package org.jboss.tools.runtime.as.ui.bot.test;

import org.jboss.tools.runtime.as.ui.bot.test.jboss71.DetectJBoss71;
import org.jboss.tools.runtime.as.ui.bot.test.jboss71.OperateJBoss71;
import org.jboss.tools.ui.bot.ext.RequirementAwareSuite;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(RequirementAwareSuite.class)
@Suite.SuiteClasses({
		DetectJBoss71.class,
		OperateJBoss71.class
})
public class AllTestsSuite {

}
