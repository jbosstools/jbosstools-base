package org.jboss.tools.ui.bot.ext;

import org.jboss.tools.ui.bot.ext.view.ConsoleView;
import org.jboss.tools.ui.bot.ext.view.PackageExplorer;
import org.jboss.tools.ui.bot.ext.view.ProblemsView;
import org.jboss.tools.ui.bot.ext.view.ProjectExplorer;
import org.jboss.tools.ui.bot.ext.view.ServersView;

/**
 * Provide factory for bot extensions, bot views and other parts. If needed
 * getter methods can be re-implemented to avoid specific bot issues
 * 
 * @author jpeterka
 * 
 */
public class SWTBotFactory {
	protected static final SWTBotExt bot = new SWTBotExt();
	protected static final SWTEclipseExt eclipse = new SWTEclipseExt(bot);
	protected static final SWTUtilExt util = new SWTUtilExt(bot);
	protected static final SWTOpenExt open = new SWTOpenExt(bot);
	protected static final SWTJBTExt jbt = new SWTJBTExt(bot);

	// Views
	protected static final PackageExplorer packageExplorer = new PackageExplorer();
	protected static final ProjectExplorer projectExplorer = new ProjectExplorer();
	protected static final ServersView servers = new ServersView();
	protected static final ProblemsView problems = new ProblemsView();
	protected static final ConsoleView console = new ConsoleView();

	public static SWTBotExt getBot() {
		return bot;
	}

	public static SWTEclipseExt getEclipse() {
		return eclipse;
	}

	public static SWTUtilExt getUtil() {
		return util;
	}

	public static SWTOpenExt getOpen() {
		return open;
	}

	public static SWTJBTExt getJbt() {
		return jbt;
	}

	public static PackageExplorer getPackageexplorer() {
		return packageExplorer;
	}

	public static ProjectExplorer getProjectexplorer() {
		return projectExplorer;
	}

	public static ServersView getServers() {
		return servers;
	}

	public static ProblemsView getProblems() {
		return problems;
	}

	public static ConsoleView getConsole() {
		return console;
	}
}
