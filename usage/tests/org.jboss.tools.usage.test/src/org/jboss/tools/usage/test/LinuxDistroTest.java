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
		ILinuxDistro distro = new FedoraLinuxDistroFake();
		assertEquals("13", distro.getVersion());
	}

	public class FedoraLinuxDistroFake extends LinuxDistro {

		public FedoraLinuxDistroFake() {
			super(ILinuxDistro.FEDORA.getName(), "dummy");
		}

		@Override
		protected String getDistroFileContent(String filePath) throws IOException {
			return "Fedora release 13 (Goddard)";
		}
	}
}
