package org.jboss.tools.runtime.as.ui.bot.test;

import org.jboss.tools.runtime.as.ui.bot.test.detector.RuntimeDuplications;
import org.jboss.tools.runtime.as.ui.bot.test.detector.ServerWithSeam;
import org.jboss.tools.ui.bot.ext.RequirementAwareSuite;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(RequirementAwareSuite.class)
@Suite.SuiteClasses({
		RuntimeDuplications.class,
		ServerWithSeam.class
//		DetectJBoss7.class,
//		OperateJBoss7.class,
//		DetectEAP6.class,
//		OperateEAP6.class,
//		DetectEAP5.class, 
//		CheckEAP5Seam.class,
//		OperateEAP5.class, 
//		DetectEAP4.class, 
//		CheckEAP4Seam.class,
//		OperateEAP4.class,
//		DetectEPP4.class, 
//		CheckEPP4Seam.class,
//		OperateEPP4.class,
//		DetectEPP5.class, 
//		CheckEPP5Seam.class,
//		OperateEPP5.class,
//		DetectEWP5.class, 
//		CheckEWP5Seam.class,
//		OperateEWP5.class,
//		DetectSOAP5.class, 
//		CheckSOAP5Seam.class,
//		OperateSOAP5.class, 
//		DetectSOAPStandalone5.class, 
//		OperateSOAPStandalone5.class,
//		DetectSeam22.class,
//		CheckSeam22.class,
//		DetectSeam23.class,
//		CheckSeam23.class
})
public class AllTestsSuite {

}
