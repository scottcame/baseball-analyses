# Script to produce graphics used in blog post on backwards pitching

library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)

# use pitchRx::scrape() to download data in reasonable-sized chunks, and serialize them to the PITCHRX_DATA_DIRECTORY

PITCHRX_DATA_DIRECTORY = '/opt/data/baseball/'

years <- c('2018', '2017', '2016', '2015', '2014')

dfs <- map(years, function(yr) {

  games <- readRDS(file.path(PITCHRX_DATA_DIRECTORY, paste0('pitchRx', yr, '.rds'))) %>%
    mutate(datetime=ymd_hm(time_date), date=as_date(datetime)) %>%
    mutate(gameday_link=paste0('gid_', gameday_link)) %>%
    select(gameday_link, away_code, home_code, away_team_name, home_team_name, date)
  
  detail <- readRDS(file.path(PITCHRX_DATA_DIRECTORY, paste0('pitchRx', yr, '_1.rds')))
  
  extractAtBats <- function(dayList) {
    ret <- NULL
    if (!is.null(dayList$atbat)) {
      ret <- dayList$atbat %>% mutate_if(is.factor, as.character) %>%
        as_tibble() %>%
        select(gameday_link, num, inning, inning_side, pitcher, batter, pitcher_name, batter_name, p_throws, stand, atbat_des, event)
    }
    ret
  }
  
  extractPitches <- function(dayList) {
    ret <- NULL
    if (!is.null(dayList$pitch)) {
      ret <- dayList$pitch %>% mutate_if(is.factor, as.character) %>%
        as_tibble() %>%
        select(gameday_link, num, inning, inning_side, tfs_zulu, count, result=type, pitch_type, start_speed, des, zone, spin_rate, px, pz)
    }
    ret
  }
  
  extractActions <- function(dayList) {
    ret <- NULL
    if (!is.null(dayList$action)) {
      ret <- dayList$action %>% mutate_if(is.factor, as.character) %>%
        as_tibble() %>%
        select(gameday_link, num, inning, inning_side, tfs_zulu, des, event, b, s, o)
    }
    ret
  }
  
  atBats <- map_dfr(detail, extractAtBats)
  pitches <- map_dfr(detail, extractPitches)
  actions <- map_dfr(detail, extractActions)
  
  if (file.exists(file.path(PITCHRX_DATA_DIRECTORY, paste0('pitchRx', yr, '_2.rds')))) {
    detail <- readRDS(file.path(PITCHRX_DATA_DIRECTORY, paste0('pitchRx', yr, '_2.rds')))
    atBats <- atBats %>% bind_rows(map_dfr(detail, extractAtBats))
    pitches <- pitches %>% bind_rows(map_dfr(detail, extractPitches))
    actions <- actions %>% bind_rows(map_dfr(detail, extractActions))
  }
  
  games <- games %>% semi_join(pitches, by='gameday_link')
  
  pitches <- pitches %>% as_tibble() %>%
    mutate(count=as.character(count)) %>%
    mutate(
      regular_pitch=pitch_type %in% c('FA','FF','FT','FC','FS','SI','SL','CU','KC','EP','CH','SC','KN'),
      pitch_type=case_when(
        pitch_type=='FA' ~ 'Fastball',
        pitch_type=='FF' ~ 'Four-seam Fastball',
        pitch_type=='FT' ~ 'Two-seam Fastball',
        pitch_type=='FC' ~ 'Cut Fastball',
        pitch_type=='FS' ~ 'Split-Fingered Fastball',
        pitch_type=='FO' ~ 'Pitch Out',
        pitch_type=='PO' ~ 'Pitch Out',
        pitch_type=='SI' ~ 'Sinker',
        pitch_type=='SL' ~ 'Slider',
        pitch_type=='CU' ~ 'Curveball',
        pitch_type=='KC' ~ 'Knuckle-curve',
        pitch_type=='EP' ~ 'Eephus',
        pitch_type=='CH' ~ 'Changeup',
        pitch_type=='SC' ~ 'Screwball',
        pitch_type=='KN' ~ 'Knuckleball',
        pitch_type=='UN' ~ 'Unidentified',
        pitch_type=='IN' ~ 'Intentional Ball',
        pitch_type=='AB' ~ NA_character_,
        TRUE ~ pitch_type
      ))
  
  list(games=games, pitches=pitches, atBats=atBats, actions=actions)
  
})

games <- map_dfr(dfs, function(yearList) { yearList$games })
atBats <- map_dfr(dfs, function(yearList) { yearList$atBats })
pitches <- map_dfr(dfs, function(yearList) { yearList$pitches }) %>%
  mutate(tfs_zulu=na_if(tfs_zulu, '')) %>% mutate(tfs_zulu=as_datetime(tfs_zulu))
actions <- map_dfr(dfs, function(yearList) { yearList$actions }) %>%
  mutate(num=as.numeric(num))

rm(dfs)

nonAbOuts <- actions %>% filter(grepl(x=event, pattern='Caught Stealing|Picked off|Pickoff [123]|Runner Out'))

# clean up pitches and at-bats where a pitcher substituted after a 0-0 count

midAbPitchingChanges <- actions %>% filter(event=='Pitching Substitution') %>% filter(b+s > 0) %>% mutate(tfs_zulu=na_if(tfs_zulu, ''))

noTs <- nrow(filter(midAbPitchingChanges, is.na(tfs_zulu)))
if (noTs) warning(paste0(noTs, ' mid AB pitch change records had no timestamp, so those ABs will not be edited.  Nothing we can do!'))

regex <- '.+: (.+) replaces (.+)\\..*'
midAbPitchingChanges <- midAbPitchingChanges %>% filter(!is.na(tfs_zulu)) %>% mutate(tfs_zulu=as_datetime(tfs_zulu)) %>%
  mutate(Reliever=gsub(x=des, pattern=regex, replacement='\\1'), Relieved=gsub(x=des, pattern=regex, replacement='\\2')) %>%
  mutate(Relieved=gsub(x=Relieved, pattern='(.+),.+', replacement='\\1')) %>%
  mutate_at(.vars=vars(Reliever, Relieved), .funs=gsub, pattern='Jr$', replacement='Jr.') %>%
  mutate_at(.vars=vars(Reliever, Relieved), .funs=gsub, pattern='([A-Z]\\.)[ ]+([A-Z]\\.)[ ]+', replacement='\\1\\2 ') %>%
  mutate(Reliever=case_when(Reliever=='Daniel Coulombe' ~ 'Danny Coulombe', TRUE ~ Reliever)) %>%
  mutate(Reliever=case_when(Reliever=='Felipe Rivero' ~ 'Felipe Vazquez', TRUE ~ Reliever)) %>%
  filter(Reliever != Relieved)

players <- pitchRx::scrape(game.ids=unique(midAbPitchingChanges$gameday_link), suffix='players.xml')
players <- players$player %>% as_tibble() %>% mutate(fullName=paste0(first, ' ', last))

midAbPitchingChanges <- midAbPitchingChanges %>% inner_join(players %>% select(fullName, RelievedId=id, gameday_link) %>% distinct(), by=c('Relieved'='fullName', 'gameday_link'))

retainAtBats <- atBats %>% anti_join(midAbPitchingChanges, by=c('gameday_link', 'num')) %>% mutate(num_seq=1)
retainPitches <- pitches %>% anti_join(midAbPitchingChanges, by=c('gameday_link', 'num')) %>% mutate(num_seq=1)
retainNonAbOuts <- nonAbOuts %>% anti_join(midAbPitchingChanges, by=c('gameday_link', 'num')) %>% mutate(num_seq=1)

fixAtBats <- atBats %>% semi_join(midAbPitchingChanges, by=c('gameday_link', 'num'))
fixPitches <- pitches %>% semi_join(midAbPitchingChanges, by=c('gameday_link', 'num'))
fixNonAbOuts <- nonAbOuts %>% semi_join(midAbPitchingChanges, by=c('gameday_link', 'num'))

fixAtBatsFixed <- fixAtBats %>% mutate(num_seq=2) %>%
  bind_rows(fixAtBats %>% select(-pitcher, -pitcher_name) %>%
              inner_join(midAbPitchingChanges %>% select(gameday_link, num, pitcher=RelievedId, pitcher_name=Relieved), by=c('gameday_link', 'num')) %>% mutate(num_seq=1))

fixPitchesFixed <- fixPitches %>% inner_join(midAbPitchingChanges %>% select(gameday_link, num, PitchChangeTime=tfs_zulu), by=c('gameday_link', 'num')) %>%
  filter(tfs_zulu < PitchChangeTime) %>% select(-PitchChangeTime) %>% mutate(num_seq=1) %>%
  bind_rows(fixPitches %>% inner_join(midAbPitchingChanges %>% select(gameday_link, num, PitchChangeTime=tfs_zulu), by=c('gameday_link', 'num')) %>%
              filter(tfs_zulu >= PitchChangeTime) %>% select(-PitchChangeTime) %>% mutate(num_seq=2))

fixNonAbOutsFixed <- fixNonAbOuts %>% inner_join(midAbPitchingChanges %>% select(gameday_link, num, PitchChangeTime=tfs_zulu), by=c('gameday_link', 'num')) %>%
  filter(tfs_zulu < PitchChangeTime) %>% select(-PitchChangeTime) %>% mutate(num_seq=1) %>%
  bind_rows(fixNonAbOuts %>% inner_join(midAbPitchingChanges %>% select(gameday_link, num, PitchChangeTime=tfs_zulu), by=c('gameday_link', 'num')) %>%
              filter(tfs_zulu >= PitchChangeTime) %>% select(-PitchChangeTime) %>% mutate(num_seq=2))

pitches <- bind_rows(retainPitches, fixPitchesFixed)
atBats <- bind_rows(retainAtBats, fixAtBatsFixed)
nonAbOuts <- bind_rows(retainNonAbOuts, fixNonAbOutsFixed)
rm(midAbPitchingChanges, fixAtBats, fixAtBatsFixed, fixPitches, fixPitchesFixed, retainAtBats, retainPitches, actions, retainNonAbOuts, fixNonAbOutsFixed, fixNonAbOuts)

games <- games %>% mutate(date=case_when(
  is.na(date) ~ ymd(gsub(x=gsub(x=gameday_link, pattern='gid_([0-9_]+)_[a-z].+', replacement='\\1'), pattern='_', replacement='-')),
  TRUE ~ date
)) %>% group_by(gameday_link) %>% filter(date==max(date)) %>% filter(row_number()==1)

atBats <- atBats %>% inner_join(games, by='gameday_link') %>%
  mutate(
    defense_team_code=case_when(inning_side=='top' ~ home_code, TRUE ~ away_code),
    defense_team_name=case_when(inning_side=='top' ~ home_team_name, TRUE ~ away_team_name),
    offense_team_code=case_when(inning_side=='bottom' ~ home_code, TRUE ~ away_code),
    offense_team_name=case_when(inning_side=='bottom' ~ home_team_name, TRUE ~ away_team_name)
  ) %>%
  mutate(outsRecorded=case_when(
    event %in% c('Strikeout', 'Groundout', 'Flyout', 'Lineout', 'Pop Out', 'Forceout', 'Sac Fly', 'Sac Bunt', 'Runner Out', 'Bunt Groundout', 'Fielders Choice Out', 'Bunt Pop Out',
                 'Batter Interference', 'Bunt Lineout') ~ 1,
    event %in% c('Grounded Into DP', 'Double Play', 'Strikeout - DP', 'Sac Fly DP') ~ 2,
    event == 'Triple Play' ~ 3,
    TRUE ~ 0)
  )

nonAbOuts <- nonAbOuts %>% inner_join(atBats %>% select(gameday_link, num, pitcher), by=c('gameday_link', 'num'))

rm(games)

pitches2 <- pitches %>% mutate(FirstPitch=count=='0-0') %>%
  mutate(Fastball=case_when(
    pitch_type %in% c('Fastball', 'Four-seam Fastball', 'Two-seam Fastball') ~ TRUE,
    is.na(pitch_type) ~ NA,
    TRUE ~ FALSE
  )) %>% inner_join(atBats %>% select(-inning, -inning_side), by=c('gameday_link', 'num', 'num_seq'))

pitchMonth <- pitches2 %>% filter(!is.na(date)) %>% filter(!(month(date) %in% c(3, 10, 11))) %>% filter(regular_pitch) %>%
  mutate(ym=format(date, '%Y-%m'), FirstFastball=case_when(FirstPitch ~ Fastball, TRUE ~ NA)) %>% group_by(ym) %>%
  summarize(FirstFastball=mean(FirstFastball, na.rm=TRUE), Fastball=mean(Fastball, na.rm=TRUE), pitches=n())

ggplot(pitchMonth) +
  geom_line(aes(x=ym, y=Fastball, group=1), color='red') +
  geom_line(aes(x=ym, y=FirstFastball, group=1), color='green') +
  scale_y_continuous(limits=c(.2, .6), labels=percent) +
  scale_x_discrete(breaks=paste0(rep(2014:2018, each=2), '-', c('05','08'))) +
  labs(y='% fastballs', x=NULL)

ggplot(pitchMonth) + geom_line(aes(x=ym, y=pitches, group=1))

pitches %>% mutate(y=year(tfs_zulu), m=month(tfs_zulu)) %>% filter(y==2016) %>% filter(m %in% 4:9) %>% filter(regular_pitch) %>%
  ggplot() + geom_bar(aes(x=pitch_type)) + coord_flip() + facet_wrap(vars(m))

## Some 2018 by-pitcher analyses...

# first have to correct for some missing pitcher names in pitchFx...
pdf <- read_csv('/opt/data/retrosheet/people.csv') %>% transmute(pitcher=key_mlbam, pitcher_name_mlbam=paste0(name_first, ' ', name_last))

pitches2 <- pitches %>% mutate(FirstPitch=count=='0-0') %>%
  mutate(Fastball=case_when(
    pitch_type %in% c('Fastball', 'Four-seam Fastball', 'Two-seam Fastball', 'Cut Fastball', 'Sinker') ~ TRUE,
    is.na(pitch_type) ~ NA,
    TRUE ~ FALSE
  )) %>% inner_join(atBats %>% select(-inning, -inning_side), by=c('gameday_link', 'num', 'num_seq')) %>%
  mutate(y=year(tfs_zulu)) %>% 
  left_join(pdf, by='pitcher') %>%
  mutate(pitcher_name=case_when(is.na(pitcher_name) ~ pitcher_name_mlbam, TRUE ~ pitcher_name)) %>% select(-pitcher_name_mlbam) %>%
  mutate(pitcher_name=case_when(
    pitcher_name=='CC Sabathia' ~ 'C.C. Sabathia',
    pitcher_name=='Douglas Fister' ~ 'Doug Fister',
    pitcher_name=='Hyunjin Ryu' ~ 'Hyun-Jin Ryu',
    pitcher_name=='Michael Fiers' ~ 'Mike Fiers',
    pitcher_name=='Jacob Faria' ~ 'Jake Faria',
    TRUE ~ pitcher_name
  ))

listTeams <- function(teamCol) {
  paste0(unique(toupper(teamCol)), collapse='/')
}

pitcherOuts <- atBats %>% filter(outsRecorded != 0) %>% select(date, pitcher, pitcher_name, outsRecorded) %>%
  bind_rows(nonAbOuts %>% select(tfs_zulu, pitcher) %>% mutate(date=as_date(tfs_zulu), outsRecorded=1) %>% select(-tfs_zulu) %>%
              inner_join(pdf %>% select(pitcher, pitcher_name=pitcher_name_mlbam), by='pitcher')) %>%
  mutate(y=year(date))

pitcherWalkHits <- atBats %>% mutate(WH=event %in% c('Walk', 'Single', 'Double', 'Triple', 'Home Run'), y=year(date)) %>%
  select(y, pitcher, pitcher_name, WH) %>% filter(WH)

totalSum <- pitches2 %>% group_by(y, pitcher, pitcher_name) %>% summarize(BF=sum(FirstPitch), AllFastballs=mean(Fastball, na.rm=TRUE), Team=listTeams(defense_team_code)) %>%
  inner_join(pitches2 %>% group_by(y, pitcher, pitcher_name) %>% filter(FirstPitch) %>% summarize(FirstPitchFastballs=mean(Fastball, na.rm=TRUE)), by=c('y', 'pitcher', 'pitcher_name')) %>%
  inner_join(pitches2 %>% group_by(y, pitcher, pitcher_name, gameday_link) %>%
               mutate(fi=inning==1) %>%
               summarize(StartedGame=max(fi, na.rm=TRUE)) %>%
               group_by(y, pitcher, pitcher_name) %>% summarize(PctStart=mean(StartedGame, na.rm=TRUE)), by=c('y', 'pitcher', 'pitcher_name')) %>%
  inner_join(pitcherOuts %>% group_by(y, pitcher, pitcher_name) %>% summarize(OutsRecorded=sum(outsRecorded)), by=c('y', 'pitcher', 'pitcher_name')) %>%
  inner_join(pitcherWalkHits %>% group_by(y, pitcher, pitcher_name) %>% summarize(WH=n()), by=c('y', 'pitcher', 'pitcher_name')) %>%
  filter(BF >= 30) %>% mutate(Starter=PctStart >= .8) %>% mutate(PitcherType=case_when(Starter ~ 'Starters', TRUE ~ 'Relievers')) %>% mutate(BackwardsRatio=AllFastballs/FirstPitchFastballs) %>%
  mutate(PitcherNameTeam=paste0(pitcher_name, ' (', Team, ')')) %>%
  mutate(WHIP=WH*3/OutsRecorded) %>%
  ungroup()

pitches2 %>% filter(y==2018) %>% summarize(Pitches=n(), Fastballs=mean(Fastball, na.rm=TRUE)) %>% bind_cols(
  pitches2 %>% filter(y==2018) %>% filter(FirstPitch) %>% summarize(FirstPitches=n(), FirstPitchFastballs=mean(Fastball, na.rm=TRUE))
) %>% bind_cols(
  pitches2 %>% filter(y==2018) %>% filter(!FirstPitch) %>% summarize(NonFirstPitches=n(), NonFirstPitchFastballs=mean(Fastball, na.rm=TRUE))
)

integerPercent <- function(v) {percent(v) %>% gsub(x=., pattern='([0-9]+)[^0-9].+\\%', replacement='\\1%')}

totalSum %>% filter(y==2018) %>%
  ggplot() +
  geom_point(aes(x=AllFastballs, y=FirstPitchFastballs, color=PitcherType)) + geom_abline(slope=1, intercept=0, linetype=2, color='grey50') +
  geom_label_repel(data=totalSum %>% filter(y==2018) %>% group_by(PitcherType) %>% top_n(wt=BackwardsRatio, n=5),
                   mapping=aes(x=AllFastballs, y=FirstPitchFastballs, label=PitcherNameTeam), nudge_x = .2, nudge_y=-0.15, force=0.1, segment.alpha = .2, size=2.75) +
  # geom_label_repel(data=totalSum %>% filter(y==2018) %>% filter(Team=='SEA') %>% filter(BackwardsRatio <= 1),
  #                  mapping=aes(x=AllFastballs, y=FirstPitchFastballs, label=PitcherNameTeam), nudge_x = -0.2, nudge_y=0.15, force=0.1, segment.alpha = .2, size=2.75) +
  # geom_label_repel(data=totalSum %>% filter(y==2018) %>% filter(Team=='SEA') %>% filter(BackwardsRatio > 1),
  #                  mapping=aes(x=AllFastballs, y=FirstPitchFastballs, label=PitcherNameTeam), nudge_x = 0.2, nudge_y=-0.15, force=0.1, segment.alpha = .2, size=2.75) +
  scale_x_continuous(labels=integerPercent, limits=c(0,1)) + scale_y_continuous(labels=integerPercent, limits=c(0,1)) +
  annotate('text', x=.15, y=.1, label='\u2193 Relatively Fewer First-Pitch Fastballs', size=3, alpha=.9, hjust='left') +
  annotate('text', x=.025, y=.30, label='\u2191 Relatively More First-Pitch Fastballs', size=3, alpha=.9, hjust='left') +
  theme_bw() +
  labs(color=NULL, x='% of all pitches that were fastballs', y='% of first pitches that were fastballs',
       title='"Backwards" Pitching on the First Pitch to Each Batter',
       subtitle='2018 Season Prior to the All-Star Break, Pitchers with 30 or More Batters Faced',
       caption='Source: MLBAM PitchFx Data\n"Fastball" is defined as a pitch identified by PitchFx as a 4-seam, 2-seam, or cut fastball, or sinker')

totalSum %>% filter(y==2018) %>% summarize(AllPitchers=n(), BackwardsPitchers=sum(BackwardsRatio > 1))

totalSum %>% filter(y==2018) %>%
  ggplot() +
  geom_point(aes(x=BackwardsRatio, y=WHIP, color=PitcherType)) +
  geom_label_repel(data=totalSum %>% filter(y==2018) %>% filter(BackwardsRatio > 1) %>% filter(WHIP < 1) %>% filter(PitcherType=='Starters'),
                   mapping=aes(x=BackwardsRatio, y=WHIP, label=PitcherNameTeam), nudge_y=0.3, nudge_x=0.3, force=.5, segment.alpha = .2, size=2.75) +
  geom_label_repel(data=totalSum %>% filter(y==2018) %>% filter(BackwardsRatio > 1) %>% filter(WHIP < 1) %>% filter(PitcherType=='Relievers'),
                   mapping=aes(x=BackwardsRatio, y=WHIP, label=PitcherNameTeam), nudge_y=-0.4, nudge_x=0.3, force=.5, segment.alpha = .2, size=2.75) +
  geom_vline(xintercept = 1, alpha=.5, linetype='dashed') +
  theme_bw() +
  annotate('text', x=.98, y=3.4, label='Relatively More First-Pitch Fastballs  \u2190', size=3, alpha=.9, hjust='right') +
  annotate('text', x=1.02, y=3.4, label='\u2192  Relatively Fewer First-Pitch Fastballs', size=3, alpha=.9, hjust='left') +
  scale_y_continuous(limits=c(0,3.5)) +
  labs(color=NULL, x='First-Pitch-Backwards Ratio (Fastball Percentage / First-Pitch Fastball Percentage)', y='Walks + Hits per Inning Pitched',
       title='Effectiveness of "Backwards" Pitching on the First Pitch to Each Batter',
       subtitle='2018 Season Prior to the All-Star Break, Pitchers with 30 or More Batters Faced',
       caption='Source: MLBAM PitchFx Data\n"Fastball" is defined as a pitch identified by PitchFx as a 4-seam, 2-seam, or cut fastball, or sinker')

strikeSum <- pitches2 %>% filter(y==2018) %>%
  mutate(Strike=!(des %in% c('Automatic Ball', 'Ball', 'Ball In Dirt', 'Hit By Pitch', 'Intent Ball', 'Pitchout')), NonFastball=!Fastball, NonFastballStrike=NonFastball*Strike) %>%
  group_by(y, pitcher, pitcher_name) %>%
  summarize(NonFastball=sum(NonFastball, na.rm=TRUE), NonFastballStrikes=sum(NonFastballStrike, na.rm=TRUE), Pitches=n(), Strikes=sum(Strike), Team=listTeams(defense_team_code)) %>%
  mutate(NonFastballStrikePct=NonFastballStrikes/NonFastball, StrikePct=Strikes/Pitches, NonStrikeDiff=NonFastballStrikePct/StrikePct) %>%
  mutate(PitcherNameTeam=paste0(pitcher_name, ' (', Team, ')')) %>%
  inner_join(totalSum %>% select(y, pitcher, BackwardsRatio, PitcherType) , by=c('y', 'pitcher')) %>% ungroup() %>%
  filter(NonFastballStrikePct > .01)

strikeSum %>%
  ggplot() + geom_point(aes(x=BackwardsRatio, y=NonFastballStrikePct, color=PitcherType)) +
  geom_vline(xintercept = 1, alpha=.5, linetype='dashed') +
  geom_smooth(data=strikeSum %>% filter(BackwardsRatio < 1.5), mapping=aes(x=BackwardsRatio, y=NonFastballStrikePct), method = lm, se = FALSE) +
  geom_label_repel(data=strikeSum %>% top_n(wt=BackwardsRatio, n=5), mapping=aes(x=BackwardsRatio, y=NonFastballStrikePct, label=PitcherNameTeam),
                   nudge_x=0.05, nudge_y=-0.05, force=0.1, segment.alpha = .2, size=2.75) +
  geom_label_repel(data=strikeSum %>% filter(BackwardsRatio > 1) %>% top_n(wt=NonFastballStrikePct, n=5), mapping=aes(x=BackwardsRatio, y=NonFastballStrikePct, label=PitcherNameTeam),
                   nudge_x=0.4, nudge_y=0.08, force=0.1, segment.alpha = .2, size=2.75) +
  theme_bw() +
  scale_y_continuous(labels=integerPercent, limits=c(.3,.8)) +
  labs(color=NULL, x='First-Pitch-Backwards Ratio (Fastball Percentage / First-Pitch Fastball Percentage)', y='% of Non-Fastballs Thrown for Strikes',
       title='First-Pitch-Backwards Pitching Tendency and Non-Fastball Command',
       subtitle='2018 Season Prior to the All-Star Break, Pitchers with 30 or More Batters Faced',
       caption='Source: MLBAM PitchFx Data\n"Fastball" is defined as a pitch identified by PitchFx as a 4-seam, 2-seam, or cut fastball, or sinker')

cor(x=strikeSum$BackwardsRatio, y=strikeSum$NonFastballStrikePct)

strikeSum %>% filter(BackwardsRatio > 1) %>% arrange(desc(NonFastballStrikePct)) %>% print(n=10)
