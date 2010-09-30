/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import java.io.IOException;

import org.jboss.tools.usage.googleanalytics.eclipse.LinuxSystem.LinuxDistro;

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