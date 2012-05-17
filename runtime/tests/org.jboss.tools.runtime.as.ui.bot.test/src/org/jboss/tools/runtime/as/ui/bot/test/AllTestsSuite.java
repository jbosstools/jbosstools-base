package org.jboss.tools.runtime.as.ui.bot.test;

import org.jboss.tools.runtime.as.ui.bot.test.eap4.DetectEAP4;
import org.jboss.tools.runtime.as.ui.bot.test.eap4.OperateEAP4;
import org.jboss.tools.runtime.as.ui.bot.test.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.eap5.OperateEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.eap6.DetectEAP6;
import org.jboss.tools.runtime.as.ui.bot.test.eap6.OperateEAP6;
import org.jboss.tools.runtime.as.ui.bot.test.epp4.DetectEPP4;
import org.jboss.tools.runtime.as.ui.bot.test.epp4.OperateEPP4;
import org.jboss.tools.runtime.as.ui.bot.test.epp5.DetectEPP5;
import org.jboss.tools.runtime.as.ui.bot.test.epp5.OperateEPP5;
import org.jboss.tools.runtime.as.ui.bot.test.ewp5.DetectEWP5;
import org.jboss.tools.runtime.as.ui.bot.test.ewp5.OperateEWP5;
import org.jboss.tools.runtime.as.ui.bot.test.jboss71.DetectJBoss71;
import org.jboss.tools.runtime.as.ui.bot.test.jboss71.OperateJBoss71;
import org.jboss.tools.ui.bot.ext.RequirementAwareSuite;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(RequirementAwareSuite.class)
@Suite.SuiteClasses({
		DetectJBoss71.class,
		OperateJBoss71.class,
		DetectEAP6.class,
		OperateEAP6.class,
		DetectEAP5.class, 
		OperateEAP5.class, 
		DetectEAP4.class, 
		OperateEAP4.class,
		DetectEPP4.class, 
		OperateEPP4.class,
		DetectEPP5.class, 
		OperateEPP5.class,
		DetectEWP5.class, 
		OperateEWP5.class
})
public class AllTestsSuite {

}
