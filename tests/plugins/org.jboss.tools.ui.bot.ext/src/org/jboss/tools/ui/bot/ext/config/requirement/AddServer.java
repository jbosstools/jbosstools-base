package org.jboss.tools.ui.bot.ext.config.requirement;

import static org.junit.Assert.fail;

import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.config.TestConfigurator;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS42;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS50;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS51;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS6x;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS70;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossCommunityJBossAS71;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.Server.JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5x;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss42Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss50Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss51Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss6xRuntime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss70Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossCommunityJBoss71Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43Runtime;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.ServerRuntime.JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5xRuntime;
import org.jboss.tools.ui.bot.ext.gen.IServer;
import org.jboss.tools.ui.bot.ext.gen.IServerRuntime;
/**
 * adds server (version and type depends on {@link TestConfigurator#server})
 * @author lzoubek
 *
 */
public class AddServer extends RequirementBase {

	private String javaName=null;
	public AddServer() {
		String javaVer = getNeededJavaVersion(TestConfigurator.currentConfig.getServer().withJavaVersion);
		if (javaVer!=null && TestConfigurator.currentConfig.getJava()!=null &&javaVer.equals(TestConfigurator.currentConfig.getJava().version)) {
			AddJava addJava = createAddJava();
			getDependsOn().add(addJava);
			javaName=addJava.getAddedAsName();
		}
		if (TestConfigurator.currentConfig.getServer().remoteSystem!=null) {
			getDependsOn().add(createAddRemoteSystem());
		}
		
	}

	@Override
	public void handle() {
		
		// handle state when server is already configured, but configuration changed and another 
		if (SWTTestExt.configuredState.getServer().isConfigured) {
			if (SWTTestExt.configuredState.getServer().type!=TestConfigurator.currentConfig.getServer().type
					||SWTTestExt.configuredState.getServer().version!=TestConfigurator.currentConfig.getServer().version) {
				createStopServer().handle();
				createRemoveServer().handle();
			}
		}
		
		ServerInfo serverInfo = getRuntime(TestConfigurator.currentConfig.getServer().type,TestConfigurator.currentConfig.getServer().version);
		String runtimeHome=TestConfigurator.currentConfig.getServer().runtimeHome;
		String runtimeName=TestConfigurator.currentConfig.getServer().type+"-"+TestConfigurator.currentConfig.getServer().version;
		SWTTestExt.eclipse.addJbossServerRuntime(serverInfo.runtime, 
				runtimeHome, runtimeName, javaName);
		String remoteSystem = TestConfigurator.currentConfig.getServer().remoteSystem;
		String remoteHome = TestConfigurator.currentConfig.getServer().remoteHome;
		SWTTestExt.eclipse.addServer(serverInfo.server, runtimeName,remoteSystem,remoteHome);
		SWTTestExt.configuredState.getServer().isLocal = remoteSystem==null;
		SWTTestExt.configuredState.getServer().isConfigured=true;
		SWTTestExt.configuredState.getServer().name=runtimeName;
		SWTTestExt.configuredState.getServer().version=TestConfigurator.currentConfig.getServer().version;
		SWTTestExt.configuredState.getServer().type=TestConfigurator.currentConfig.getServer().type;
		SWTTestExt.configuredState.getServer().withJavaVersion = TestConfigurator.currentConfig.getServer().withJavaVersion;
		// setup bundled ESB versions for SOA server type
		if (TestConfigurator.currentConfig.getServer().type.equals(TestConfigurator.Values.SERVER_TYPE_SOA)) {
			if ("4.3".equals(TestConfigurator.currentConfig.getServer().version)) {
				SWTTestExt.configuredState.getServer().bundledESBVersion="4.4";			
			}
			else if ("5.0".equals(TestConfigurator.currentConfig.getServer().version)) {
				SWTTestExt.configuredState.getServer().bundledESBVersion="4.7";
			}
			else if ("5.1".equals(TestConfigurator.currentConfig.getServer().version)) {
				SWTTestExt.configuredState.getServer().bundledESBVersion="4.9";
			}
			else if ("5.2".equals(TestConfigurator.currentConfig.getServer().version)) {
				SWTTestExt.configuredState.getServer().bundledESBVersion="4.10";
			}
		} 
	}

	
	class ServerInfo {
		public ServerInfo(IServerRuntime runtime,IServer server) {
			this.runtime=runtime;
			this.server=server;
		}
		public IServerRuntime runtime;
		public IServer server;
	}
	private String getNeededJavaVersion(String javaVer) {
		if (TestConfigurator.Values.SERVER_WITH_DEFAULT_JAVA.equals(javaVer)) {
			return null;
		}
		else {return javaVer;}

	}
	private ServerInfo getRuntime(String serverType, String version) {
		if (TestConfigurator.Values.SERVER_TYPE_EAP.equals(serverType)) {
			if ("4.3".equals(version)) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43Runtime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43.LABEL
						);				
			}

			if (version!=null && version.startsWith("5")) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5xRuntime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5x.LABEL);
			}
			
		}
		if (TestConfigurator.Values.SERVER_TYPE_EPP.equals(serverType)) {
			if ("4.3".equals(version)) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43Runtime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43.LABEL
						);				
			}
			if (version!=null && version.startsWith("5")) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5xRuntime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5x.LABEL);
			}
			
		}
		if (TestConfigurator.Values.SERVER_TYPE_SOA.equals(serverType)) {
			if ("4.3".equals(version)) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43Runtime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43.LABEL
						);				
			}
			if (version!=null && version.startsWith("5")) {
				return new ServerInfo(JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5xRuntime.LABEL,
						JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5x.LABEL);
			}			
		}
		else if (TestConfigurator.Values.SERVER_TYPE_AS.equals(serverType)) {
			if ("4.2".equals(version)) {
				return new ServerInfo(JBossCommunityJBoss42Runtime.LABEL,JBossCommunityJBossAS42.LABEL);				
			}
			if ("5.0".equals(version)) {
				return new ServerInfo(JBossCommunityJBoss50Runtime.LABEL,JBossCommunityJBossAS50.LABEL);				
			}
			if ("5.1".equals(version)) {
				return new ServerInfo(JBossCommunityJBoss51Runtime.LABEL,JBossCommunityJBossAS51.LABEL);
			}
			if (version!=null && version.startsWith("6")) {
				return new ServerInfo(JBossCommunityJBoss6xRuntime.LABEL,JBossCommunityJBossAS6x.LABEL);				
			}
			if ("7.0".equals(version)) {
				return new ServerInfo(JBossCommunityJBoss70Runtime.LABEL,JBossCommunityJBossAS70.LABEL);
			}
			if ("7.1".equals(version)) {
				return new ServerInfo(JBossCommunityJBoss71Runtime.LABEL,JBossCommunityJBossAS71.LABEL);
			}
		}
		failParsing();
		return null;
	}
	private void failParsing() {
		fail("Unable to add server runtime, unparsable or not supported property value: "+TestConfigurator.getProperty(TestConfigurator.Keys.SERVER));
	}
	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.getServer().isConfigured 
		&& SWTTestExt.configuredState.getServer().type.equals(TestConfigurator.currentConfig.getServer().type)
		&& SWTTestExt.configuredState.getServer().version.equals(TestConfigurator.currentConfig.getServer().version);
	}

}
