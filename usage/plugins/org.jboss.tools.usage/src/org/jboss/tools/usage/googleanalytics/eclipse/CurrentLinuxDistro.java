/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.googleanalytics.eclipse;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CurrentLinuxDistro {

	public static final LinuxDistro DEBIAN = new LinuxDistro("Debian", "/etc/debian_version");
	public static final LinuxDistro FEDORA = new LinuxDistro("Fedora", "/etc/fedora-release");
	public static final LinuxDistro GENTOO = new LinuxDistro("Gentoo", "/etc/gentoo-release");
	public static final LinuxDistro KNOPPIX = new LinuxDistro("Knoppix", "knoppix_version");
	public static final LinuxDistro MANDRAKE = new LinuxDistro("Mandrake", "/etc/mandrake-release");
	public static final LinuxDistro MANDRIVA = new LinuxDistro("Mandriva", "/etc/mandriva-release");
	public static final LinuxDistro PLD = new LinuxDistro("PLD", "/etc/pld-release");
	public static final LinuxDistro REDHAT = new LinuxDistro("RedHat", "/etc/redhat-release");
	public static final LinuxDistro SLACKWARE = new LinuxDistro("Slackware", "/etc/slackware-version");
	public static final LinuxDistro SUSE = new LinuxDistro("SUSE", "/etc/SuSE-release");
	public static final LinuxDistro UBUNTU = new LinuxDistro("Ubuntu", "/etc/lsb-release");
	public static final LinuxDistro YELLOWDOG = new LinuxDistro("YellowDog", "/etc/yellowdog-release");

	private static final LinuxDistro[] ALL = new LinuxDistro[] {
				DEBIAN,
				FEDORA,
				GENTOO,
				KNOPPIX,
				MANDRAKE,
				MANDRIVA,
				PLD,
				REDHAT,
				SLACKWARE,
				SUSE,
				UBUNTU,
				YELLOWDOG
		};

	public static LinuxDistro getDistro() {
			for (LinuxDistro distro : ALL) {
				if (distro.currentSysIsDistro()) {
					return distro;
				}
			}
			return null;

		}

	public static String getNameAndVersion() {
			LinuxDistro distro = getDistro();
			if (distro != null) {
				return distro.getNameAndVersion();
			} else {
				return "";
			}
		}

	public static class LinuxDistro {

		/**
		 * The pattern to match the contents of the release-file -
		 * /etc/fedora-release etc. Attention: Ubuntu has multi-line release
		 * file
		 * 
		 * @see <a
		 *      href="http://superuser.com/questions/11008/how-do-i-find-out-what-version-of-linux-im-running">release-file
		 *      strings</a>
		 */
		private final Pattern VERSION_REGEX = Pattern.compile("([0-9.]+)");

		private final String releaseFilePath;
		private String name;

		protected LinuxDistro(String name, String releaseFilePath) {
			this.name = name;
			this.releaseFilePath = releaseFilePath;
		}

		private boolean currentSysIsDistro() {
			return new File(releaseFilePath).exists();
		}

		public String getName() {
			return name;
		}

		public String getVersion() {
			try {
				String distroString = getDistroFileContent(releaseFilePath);
				Matcher matcher = VERSION_REGEX.matcher(distroString);
				if (matcher.find()) {
					return matcher.group(1);
				}
			} catch (IOException e) {
			}
			return "";
		}

		public String getNameAndVersion() {
			return new StringBuilder().append(getName()).append(getVersion()).toString();
		}

		protected String getDistroFileContent(String filePath) throws IOException {
			int charachtersToRead = 1024;
			StringBuilder builder = new StringBuilder(charachtersToRead);
			BufferedReader reader = new BufferedReader(new FileReader(filePath));
			char[] buf = new char[charachtersToRead];
			int charRead = 0;
			while ((charRead = reader.read(buf)) != -1 && builder.length() < charachtersToRead) {
				String readData = String.valueOf(buf, 0, charRead);
				builder.append(readData);
			}
			reader.close();
			return builder.toString();
		}
	}
}
