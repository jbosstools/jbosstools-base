/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;

import org.jboss.tools.usage.googleanalytics.eclipse.CurrentLinuxDistro;
import org.jboss.tools.usage.googleanalytics.eclipse.CurrentLinuxDistro.LinuxDistro;
import org.jboss.tools.usage.test.fakes.LinuxDistroFake;
import org.junit.Test;

public class LinuxDistroTest {

	@Test
	public void canExtractFedoraVersion() {
		LinuxDistro distro = new LinuxDistroFake(CurrentLinuxDistro.FEDORA.getName(), "Fedora release 13 (Goddard)");
		assertEquals("13", distro.getVersion());
	}

	@Test
	public void canExtractUbuntuVersion() {
		LinuxDistro distro = new LinuxDistroFake(CurrentLinuxDistro.UBUNTU.getName(),
				"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=9.04\nDISTRIB_CODENAME=jaunty\nDISTRIB_DESCRIPTION=\"Ubuntu 9.04\"");
		assertEquals("9.04", distro.getVersion());
	}

	@Test
	public void canExtractREDHATVersion() {
		LinuxDistro distro = new LinuxDistroFake(CurrentLinuxDistro.REDHAT.getName(),
				"Red Hat Enterprise Linux Workstation release 6.0 (Santiago)");
		assertEquals("6.0", distro.getVersion());
	}
}
