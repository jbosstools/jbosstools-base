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

import java.io.IOException;

import org.jboss.tools.usage.googleanalytics.eclipse.ILinuxDistro;
import org.jboss.tools.usage.googleanalytics.eclipse.ILinuxDistro.LinuxDistro;
import org.junit.Test;

public class LinuxDistroTest {

	@Test
	public void canExtractFedoraVersion() {
		ILinuxDistro distro = new LinuxDistroFake(ILinuxDistro.FEDORA.getName(), "Fedora release 13 (Goddard)");
		assertEquals("13", distro.getVersion());
	}

	@Test
	public void canExtractUbuntuVersion() {
		ILinuxDistro distro = new LinuxDistroFake(ILinuxDistro.UBUNTU.getName(),
				"DISTRIB_ID=Ubuntu\nDISTRIB_RELEASE=9.04\nDISTRIB_CODENAME=jaunty\nDISTRIB_DESCRIPTION=\"Ubuntu 9.04\"");
		assertEquals("9.04", distro.getVersion());
	}

	public class LinuxDistroFake extends LinuxDistro {

		private String releaseFileContent;

		public LinuxDistroFake(String name, String releaseFileContent) {
			super(name, "dummy");
			this.releaseFileContent = releaseFileContent;
		}

		@Override
		protected String getDistroFileContent(String filePath) throws IOException {
			return releaseFileContent;
		}
	}
}
