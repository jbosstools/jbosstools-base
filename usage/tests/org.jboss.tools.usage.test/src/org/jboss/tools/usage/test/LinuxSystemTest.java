/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
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
	public void canDetectFedora() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(LinuxSystem.INSTANCE.FEDORA.getReleaseFilePath(), "Fedora release 13 (Goddard)"));
		assertEquals("Fedora 13", linuxSystem.getDistroNameAndVersion());
	}

	/**
	 * Mint uses the default
	 * <ul>
	 * <li>/etc/lsb-release</li>
	 * </ul>
	 */
	@Test
	public void canDetectMintVersion() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
					LinuxSystem.INSTANCE.DEBIAN.getReleaseFilePath(), "squeeze/sid"),
			new ReleaseFile(
				LinuxSystem.INSTANCE.MINT.getReleaseFilePath(),
				"DISTRIB_ID=LinuxMint\n" +
				"DISTRIB_RELEASE=12\n" +
				"DISTRIB_CODENAME=debian\n" +
				"DISTRIB_DESCRIPTION=\"Linux Mint 12 Lisa\"")
			);
		assertEquals("LinuxMint 12", linuxSystem.getDistroNameAndVersion());
	}

	/**
	 * Ubuntu has 2 release files!
	 * <ul>
	 * <li>/etc/lsb-release</li>
	 * <li>/etc/debian_version</li>
	 * </ul>
	 */
	@Test
	public void canDetectUbuntu() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				LinuxSystem.INSTANCE.DEBIAN.getReleaseFilePath(), "squeeze/sid"),				
			new ReleaseFile(
				LinuxSystem.INSTANCE.UBUNTU.getReleaseFilePath(),
				"DISTRIB_ID=Ubuntu\n" +
				"DISTRIB_RELEASE=9.04\n" +
				"DISTRIB_CODENAME=jaunty\n" +
				"DISTRIB_DESCRIPTION=\"Ubuntu 9.04\""));
		assertEquals("Ubuntu 9.04", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void returnsUnknownIfLSBReleaseWithUnknownContent() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				"/etc/lsb-release", 
				"adietish@redhat.com"));
		assertEquals("Unknown", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canDetectRedHat() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				LinuxSystem.INSTANCE.REDHAT.getReleaseFilePath(),
				"Red Hat Enterprise Linux Workstation release 6.0 (Santiago)"));
		assertEquals("Red Hat 6.0", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canDetectGentoo() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				LinuxSystem.INSTANCE.GENTOO.getReleaseFilePath(), 
				"Gentoo Base System release 2.0.1"));
		assertEquals("Gentoo 2.0.1", linuxSystem.getDistroNameAndVersion());
	}

	/**
	 * CentOS uses the redhat-release file!
	 * <ul>
	 * <li>/etc/redhat-release</li>
	 * </ul>
	 */
	@Test
	public void canDetectCentOS() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				LinuxSystem.INSTANCE.CENTOS.getReleaseFilePath(), 
				"CentOS release 5.3 (Final)"));
		assertEquals("CentOS 5.3", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void canDetectOsReleaseBasedDistro() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/etc/os-release",
						"NAME=\"Ubuntu\"\n" +
						"VERSION=\"15.04 (Vivid Vervet)\"\n" +
						"ID=ubuntu\n" +
						"PRETTY_NAME=\"Ubuntu 15.04\"" +
						"VERSION_ID=\"15.04\"\n" +
						"HOME_URL=\"http://www.ubuntu.com/\"\n" +
						"SUPPORT_URL=\"http://help.ubuntu.com/\"\n" +
						"BUG_REPORT_URL=\"http://bugs.launchpad.net/ubuntu/\"\n")
				);
		assertEquals("Ubuntu 15.04", linuxSystem.getDistroNameAndVersion());
		linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/etc/os-release",
						"VERSION=\"15.04 (Vivid Vervet)\"\n" +
						"ID=ubuntu\n" +
						"PRETTY_NAME=\"Ubuntu 15.04\"" +
						"VERSION_ID=\"15.04\"\n")
				);
		assertEquals("ubuntu 15.04", linuxSystem.getDistroNameAndVersion());
		linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/etc/os-release",
						"NAME=Ubuntu\n" +
						"VERSION=\"15.04 (Vivid Vervet)\"\n" +
						"ID=ubuntu\n")
				);
		assertEquals("Ubuntu 15.04", linuxSystem.getDistroNameAndVersion());
		linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/usr/lib/os-release",
						"NAME=Ubuntu\n" +
						"VERSION=\"15.04 (Vivid Vervet)\"\n" +
						"ID=ubuntu\n")
				);
		assertEquals("Ubuntu 15.04", linuxSystem.getDistroNameAndVersion());
		linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/usr/lib/os-release",
						"NAME=incorrect\n" +
						"ID=incorrect\n"),
				new ReleaseFile(
						"/etc/os-release",
						"NAME=Ubuntu\n" +
						"VERSION=\"15.04 (Vivid Vervet)\"\n" +
						"ID=ubuntu\n")
				);
		assertEquals("Ubuntu 15.04", linuxSystem.getDistroNameAndVersion());
		linuxSystem = new LinuxSystemFake(
				new ReleaseFile(
						"/etc/os-release",
						"#$%^&*blah-blah-blah")
				);
		assertEquals("Unknown", linuxSystem.getDistroNameAndVersion());
	}

	@Test
	public void returnsUnknownIfRedHatReleaseWithUnknownContent() {
		LinuxSystem linuxSystem = new LinuxSystemFake(
			new ReleaseFile(
				LinuxSystem.INSTANCE.REDHAT.getReleaseFilePath(), 
				"adietish@redhat.com"));
		assertEquals("Unknown", linuxSystem.getDistroNameAndVersion().trim());
	}
}