package org.jboss.tools.runtime.as.ui.bot.test;

import org.jboss.tools.runtime.as.ui.bot.test.seam.seam22.DetectSeam22;
import org.jboss.tools.runtime.as.ui.bot.test.seam.seam23.DetectSeam23;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap4.DetectEAP4;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap4.OperateEAP4;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap5.DetectEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap5.OperateEAP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap6.DetectEAP6;
import org.jboss.tools.runtime.as.ui.bot.test.server.eap6.OperateEAP6;
import org.jboss.tools.runtime.as.ui.bot.test.server.epp4.DetectEPP4;
import org.jboss.tools.runtime.as.ui.bot.test.server.epp4.OperateEPP4;
import org.jboss.tools.runtime.as.ui.bot.test.server.epp5.DetectEPP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.epp5.OperateEPP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.ewp5.DetectEWP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.ewp5.OperateEWP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.jboss7.DetectJBoss7;
import org.jboss.tools.runtime.as.ui.bot.test.server.jboss7.OperateJBoss7;
import org.jboss.tools.runtime.as.ui.bot.test.server.soap5.DetectSOAP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.soap5.OperateSOAP5;
import org.jboss.tools.runtime.as.ui.bot.test.server.soap5.standalone.DetectSOAPStandalone5;
import org.jboss.tools.runtime.as.ui.bot.test.server.soap5.standalone.OperateSOAPStandalone5;
import org.jboss.tools.ui.bot.ext.RequirementAwareSuite;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(RequirementAwareSuite.class)
@Suite.SuiteClasses({
		DetectJBoss7.class,
		OperateJBoss7.class,
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
		OperateEWP5.class,
		DetectSOAP5.class, 
		OperateSOAP5.class, 
		DetectSOAPStandalone5.class, 
		OperateSOAPStandalone5.class,
		DetectSeam22.class, 
		DetectSeam23.class
})
public class AllTestsSuite {

}
