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

import org.jboss.tools.usage.googleanalytics.eclipse.LinuxSystem;
import org.jboss.tools.usage.test.fakes.LinuxSystemFake;
import org.jboss.tools.usage.test.fakes.LinuxSystemFake.ReleaseFile;
import org.junit.Test;

public class LinuxSystemTest {

	@Test
	public void canExtractFedoraVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(LinuxSystem.INSTANCE.FEDORA.getReleaseFilePath(), "Fedora release 13 (Goddard)"));
		assertEquals("Fedora 13", linuxSystem.getDistroNameAndVersion());
	}

	/**
	 * Ubuntu has 2 release files!
	 * <ul>
	 * <li>/etc/lsb-release</li>
	 * <li>/etc/debian_version</li>
	 * </ul>
	 */
	@Test
	public void canExtractUbuntuVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						LinuxSystem.INSTANCE.UBUNTU.getReleaseFilePath(),
						"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=9.04\nDISTRIB_CODENAME=jaunty\nDISTRIB_DESCRIPTION=\"Ubuntu 9.04\"")
				, new ReleaseFile(LinuxSystem.INSTANCE.DEBIAN.getReleaseFilePath(), "squeeze/sid"));
		assertEquals("Ubuntu 9.04", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canExtractRedHatVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(LinuxSystem.INSTANCE.REDHAT.getReleaseFilePath(),
						"Red Hat Enterprise Linux Workstation release 6.0 (Santiago)"));
		assertEquals("RedHat 6.0", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canExtractGentooVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(LinuxSystem.INSTANCE.GENTOO.getReleaseFilePath(), "Gentoo Base System release 2.0.1"));
		assertEquals("Gentoo 2.0.1", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canExtractCentOSVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(LinuxSystem.INSTANCE.CENTOS.getReleaseFilePath(), "CentOS release 5.3 (Final)"));
		assertEquals("CentOS 5.3", linuxSystem.getDistroNameAndVersion());
	}
}
