/*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.test.config;

import static org.junit.Assert.*;

import org.jboss.tools.ui.bot.ext.config.RemoteSystemBean;
import org.jboss.tools.ui.bot.ext.config.ServerBean;
import org.junit.Test;
/**
 * Tests if Server Bean requirement configuration string is properly parsed
 * and Server Bean fields are appropriately set.
 * @author lzoubek
 *
 */
public class ConfigBeanTest {
  /**
   * Checks Remote Server Configuration
   * 
   * @throws Exception
   */
	@Test
	public void serverBeanRemote() throws Exception{
		ServerBean bean = ServerBean.fromString("EAP,5.0,1.6,/data/jboss/jboss-eap-5.0/jboss-as,jawa23.mw.lab.eng.brq.redhat.com,/opt/jboss-as");
		assertEquals(bean.type,"EAP");
		assertEquals(bean.version,"5.0");
		assertEquals(bean.withJavaVersion,"1.6");
		assertEquals(bean.runtimeHome,"/data/jboss/jboss-eap-5.0/jboss-as");
		assertEquals(bean.remoteSystem,"jawa23.mw.lab.eng.brq.redhat.com");
		assertEquals(bean.remoteHome, "/opt/jboss-as");
	}
  /**
   * Checks Local Server Configuration
   * 
   * @throws Exception
   */
	@Test
	public void serverBeanLocal() throws Exception{
		ServerBean bean = ServerBean.fromString("EAP,5.0,1.6,/data/jboss/jboss-eap-5.0/jboss-as");
		assertEquals(bean.type,"EAP");
		assertEquals(bean.version,"5.0");
		assertEquals(bean.withJavaVersion,"1.6");
		assertEquals(bean.runtimeHome,"/data/jboss/jboss-eap-5.0/jboss-as");
		assertNull(bean.remoteSystem);
		assertNull(bean.remoteHome);
	}
  /**
   * Checks Remote System Configuration
   * 
   * @throws Exception
   */

	@Test
	public void remoteSystem() throws Exception {
		RemoteSystemBean bean = RemoteSystemBean.fromString("lzoubek,jawa23.mw.lab.eng.brq.redhat.com,/home/lzoubek/.ssh/id_rsa");
		assertEquals(bean.user,"lzoubek");
		assertEquals(bean.host,"jawa23.mw.lab.eng.brq.redhat.com");
		assertEquals(bean.key,"/home/lzoubek/.ssh/id_rsa");
	}
}
